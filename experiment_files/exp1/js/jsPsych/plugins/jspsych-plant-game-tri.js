//**
// * jsPsych plugin for running Tim's plant task
jsPsych['plant-game'] = (function() {

    var plugin = {};

    plugin.create = function(params) {

        //ADD MEAN AND SD GROWTH TO THIS
        params = jsPsych.pluginAPI.enforceArray(params, ['experimental_deadline', 'experimental_distance', 'experimental_goal', 'fixed_deadline', 'fixed_distance', 'fixed_goal',
            'mean_growth_experimental_if_prioritized', 'sd_growth_experimental_if_prioritized', 'mean_growth_experimental_if_not_prioritized', 'sd_growth_experimental_if_not_prioritized',
            'mean_growth_fixed_if_prioritized', 'sd_growth_fixed_if_prioritized', 'mean_growth_fixed_if_not_prioritized', 'sd_growth_fixed_if_not_prioritized'
        ]);

        var trials = [];

        trials[0] = {};
        //Note we have to do a mathematical operation on them to convert the object from an array to a plain number object. This simplifies the data structure.
        trials[0].experimental_deadline = params.experimental_deadline * 1;
        trials[0].experimental_distance = params.experimental_distance * 1;
        trials[0].experimental_goal = params.experimental_goal * 1;

        trials[0].fixed_deadline = params.fixed_deadline * 1;
        trials[0].fixed_distance = params.fixed_distance * 1;
        trials[0].fixed_goal = params.fixed_goal * 1;

        trials[0].mean_growth_experimental_if_prioritized = params.mean_growth_experimental_if_prioritized * 1;
        trials[0].sd_growth_experimental_if_prioritized = params.sd_growth_experimental_if_prioritized * 1;

        trials[0].mean_growth_experimental_if_balanced  = params.mean_growth_experimental_if_balanced * 1;
        trials[0].sd_growth_experimental_if_balanced  = params.sd_growth_experimental_if_balanced * 1;
        
        trials[0].mean_growth_experimental_if_not_prioritized = params.mean_growth_experimental_if_not_prioritized * 1;
        trials[0].sd_growth_experimental_if_not_prioritized = params.sd_growth_experimental_if_not_prioritized * 1;

        trials[0].mean_growth_fixed_if_prioritized = params.mean_growth_fixed_if_prioritized * 1;
        trials[0].sd_growth_fixed_if_prioritized = params.sd_growth_fixed_if_prioritized * 1;

        trials[0].mean_growth_fixed_if_balanced  = params.mean_growth_fixed_if_balanced * 1;
        trials[0].sd_growth_fixed_if_balanced  = params.sd_growth_fixed_if_balanced * 1;

        trials[0].mean_growth_fixed_if_not_prioritized = params.mean_growth_fixed_if_not_prioritized * 1;
        trials[0].sd_growth_fixed_if_not_prioritized = params.sd_growth_fixed_if_not_prioritized * 1;

        trials[0].practice = params.practice;
        trials[0].trial_number = params.trial_number * 1;
        //trials.push({});

        return trials;
    }

    plugin.trial = function(display_element, trial) {

        function keydownCallback(keydownevent, canvas, ctx, scr) {
            //var canvas = document.getElementById("canvas");
            //var ctx = canvas.getContext("2d");
            //ctx.fillText(event.keyCode, 50, 300, 500);
            //ctx.fillText(scr.keyNeeded, 50, 350, 500);

            if (scr.keyNeeded == 'response' && (keydownevent.keyCode === 83 | keydownevent.keyCode === 71 | keydownevent.keyCode === 75)) {
                scr.end = new Date().getTime();
                scr.keyDown = keydownevent.keyCode;
                $(document).off('keydown');

                $(document).keyup(function() {
                    keyupCallback(event, canvas, ctx, scr);
                });
            }

            if (scr.keyNeeded == 'v' && keydownevent.keyCode === 86) {
                scr.keyDown = keydownevent.keyCode;
                $(document).off('keydown');

                $(document).keyup(function() {
                    keyupCallback(event, canvas, ctx, scr);
                });
            }

            if (keydownevent.keyCode != 83 && keydownevent.keyCode != 71 && keydownevent.keyCode != 75 && keydownevent.keyCode != 86) {
                    scr.trial_data.other_key_pressed = scr.trial_data.other_key_pressed + 1;
            };



            return scr;
        }

        function keyupCallback(keyupevent, canvas, ctx, scr) {
            //only proceeds if the key released is the key that was pressed first
            if (keyupevent.keyCode === scr.keyDown) {
                $(document).off('keyup');
                var canvas = document.getElementById("canvas");
                var ctx = canvas.getContext("2d");
                
                if (keyupevent.keyCode === 83) {
                    //ctx.fillText("a key", 100, 500, 500);
                    //ctx.fillText("Left Weeks Remaining = " + scr.leftWeeksRemaining, 100, 700, 500);

                    //scr.end = new Date().getTime();
                    scr.responseTime = scr.end - scr.start;
                    scr.response = 0;

                    //ctx.fillText(scr.responseTime, 50, 100, 500);
                    //ctx.fillText(scr.response, 50, 200, 500);
                    //ctx.fillText(scr.leftWeeksRemaining, 50, 300, 500);
                    //ctx.fillText(scr.rightWeeksRemaining, 50, 400, 500);

                    if (scr.preTrialScreen === 0 && scr.feedbackPresented === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining > 0) {
                        scr = determineConsequences(scr);
                    }


                    //If left deadline is up, but not right, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining > 0 && scr.deadlinePresented === 0) {
                        deadlineLeft(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If right deadline is up, but not left, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 0) {
                        deadlineRight(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If both deadlines are up, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 0) {
                        deadlineBoth(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If neither deadline is up
                    if (scr.preTrialScreen === 0 && scr.feedbackPresented === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining > 0) {
                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        //ctx.fillText(scr.responseTime,100,100,100);
                    }

                }

                if (keyupevent.keyCode === 75) {
                    //scr.end = new Date().getTime();
                    scr.responseTime = scr.end - scr.start;
                    scr.response = 1;

                    if (scr.preTrialScreen === 0 && scr.feedbackPresented === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining > 0) {
                        scr = determineConsequences(scr);
                    }

                    //If left deadline is up, but not right, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining > 0 && scr.deadlinePresented === 0) {
                        deadlineLeft(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If right deadline is up, but not left, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 0) {
                        deadlineRight(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If both deadlines are up, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 0) {
                        deadlineBoth(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If neither deadline is up
                    if (scr.preTrialScreen === 0 && scr.feedbackPresented === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining > 0) {
                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        //ctx.fillText(scr.responseTime,100,100,100);
                    }

                }

                if (keyupevent.keyCode === 71) {
                    //scr.end = new Date().getTime();
                    scr.responseTime = scr.end - scr.start;
                    scr.response = 2;

                    if (scr.preTrialScreen === 0 && scr.feedbackPresented === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining > 0) {
                        scr = determineConsequences(scr);
                    }

                    //If left deadline is up, but not right, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining > 0 && scr.deadlinePresented === 0) {
                        deadlineLeft(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If right deadline is up, but not left, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 0) {
                        deadlineRight(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If both deadlines are up, and deadline has not yet been presented
                    if (scr.preTrialScreen === 0 && scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 0) {
                        deadlineBoth(canvas, ctx, scr);
                        scr.deadlinePresented = 1;
                        scr.keyNeeded = 'v';
                    }

                    //If neither deadline is up
                    if (scr.preTrialScreen === 0 && scr.feedbackPresented === 0 && scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining > 0) {
                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        //ctx.fillText(scr.responseTime,100,100,100);
                    }

                }

                if (keyupevent.keyCode === 86) {

                    //Begin Trial
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 4) {
                        scr.keyNeeded = 'response';
                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.preTrialScreen = 0;
                        scr.keyNeeded = 'response';


                    }


                    //Practice Trial Screen 5
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 3) {
                        var w = canvas.width;
                        var h = canvas.height;
                        ctx.fillStyle = "black";
                        ctx.clearRect(.01 * w, .01 * h, .98 * w, .98 * h);
                        ctx.font = "40px Times New Roman";
                        ctx.fillText("Press the 'v' key to begin the trial.", .5 * w, .5 * h, .6 * w);

                        scr.preTrialScreen = 4;

                    }

                    //Practice Trial Screen 2
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 2) {
                        drawDecisionScreen(canvas, ctx, scr);

                        var w = canvas.width;
                        var h = canvas.height;
                        ctx.fillStyle = "white";
                        ctx.clearRect(.01 * w, .95 * h, .98 * w, .04 * h);
                        //ctx.clearRect(.65 * w, .84 * h, .34 * w, .15 * h);
                        //ctx.clearRect(.25 * w, .73 * h, .5 * w, .26 * h);

                        ctx.strokeStyle = "gold";
                        ctx.strokeRect(.01 * w, .65 * h, .98 * w, .31 * h);
                        //ctx.strokeRect(.65 * w, .47 * h, .34 * w, .37 * h);

                        ctx.fillStyle = "brown";
                        ctx.lineWidth = 3;
                        ctx.font = "28px Times New Roman";
                        ctx.fillText("Here are the growth estimates", .18 * w, .52 * h, .32 * w);
                        ctx.fillText("for each fertiliser.", .18 * w, .56 * h, .32 * w);
                        ctx.font = "20px Times New Roman";
                        ctx.fillText("Press the 'v' key to continue.", .18 * w, .62 * h, .3 * w);


                        scr.preTrialScreen = 3;

                    }

                    //Practice Trial Screen 1
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 1) {
                        drawDecisionScreen(canvas, ctx, scr);

                        var w = canvas.width;
                        var h = canvas.height;
                        ctx.fillStyle = "white";
                        ctx.clearRect(.01 * w, .47 * h, .34 * w, .52 * h);
                        ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
                        ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);
                        //ctx.clearRect(.25 * w, .73 * h, .5 * w, .26 * h);

                        ctx.strokeStyle = "gold";
                        ctx.strokeRect(.02 * w, .08 * h, .32 * w, .4 * h);
                        ctx.strokeRect(.66 * w, .08 * h, .32 * w, .4 * h);

                        ctx.fillStyle = "brown";
                        ctx.lineWidth = 3;
                        ctx.font = "28px Times New Roman";
                        ctx.fillText("Here are the number of weeks", .18 * w, .51 * h, .32 * w);
                        ctx.fillText("in each growing season.", .18 * w, .55 * h, .32 * w);
                        ctx.font = "20px Times New Roman";
                        ctx.fillText("Press the 'v' key to continue.", .18 * w, .61 * h, .3 * w);

                        scr.preTrialScreen = 2;
                    }

                    //If fillers have just been introduced (which happens in the practice trial), draw decision screen and reset deadline and feedback screens
                    if (scr.feedbackPresented === 3) {
                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.feedbackPresented = 0;
                        scr.keyNeeded = 'response';
                    }

                    //If all necessary feedback has been presented, and right crop has weeks remaining
                    if (scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining > 0 && scr.feedbackPresented === 2) {

                        scr.leftWeeksRemaining = scr.fillerWeeksTotal;
                        scr.leftCurrentHeight = scr.fillerStartHeight;

                        //If first filler crop has started
                        if (scr.leftCropType === scr.firstFillerCropType) {
                            scr.leftCropType = scr.secondFillerCropType;
                            scr.leftCropColor = scr.secondFillerCropColor;
                            scr.leftGoalType = 'SecondFiller';
                        }

                        //If first filler crop has not already started
                        if ((scr.leftCropType != scr.firstFillerCropType) && (scr.leftCropType != scr.secondFillerCropType)) {
                            scr.leftCropType = scr.firstFillerCropType;
                            scr.leftCropColor = scr.firstFillerCropColor;
                            scr.leftGoalType = 'FirstFiller';
                        }

                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.deadlinePresented = 0;
                        scr.feedbackPresented = 0;
                        scr.keyNeeded = 'response';

                        if (scr.trialType === 'practice') {
                            var w = canvas.width;
                            var h = canvas.height;

                            ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);
                            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);

                            // var t1 = ctx.measureText("In addition to your ").width;
                            // var t2 = ctx.measureText(scr.rightCropType).width;
                            // var t3 = ctx.measureText(" crop, you must now manage a ").width;
                            // var t4 = ctx.measureText(scr.leftCropType).width;
                            // var t5 = ctx.measureText(" crop.").width;
                            // var totalLength = t1 + t2 + t3 + t4 + t5;


                            // ctx.fillStyle = "black";
                            // ctx.fillText("In addition to your ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = scr.rightCropColor;
                            // ctx.fillText(scr.rightCropType, .5 * w - totalLength / 2 + t1 + t2 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = "black";
                            // ctx.fillText(" crop, you must now manage a ", .5 * w - totalLength / 2 + t1 + t2 + t3 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = scr.leftCropColor;
                            // ctx.fillText(scr.leftCropType, .5 * w - totalLength / 2 + t1 + t2 + t3 + t4 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = "black";
                            // ctx.fillText(" crop.", .5 * w - totalLength / 2 + t1 + t2 + t3 + t4 + t5 / 2, .8 * h, .6 * w);

                            // ctx.font = "20px Times New Roman";
                            // ctx.fillText("Press the 'v' key to continue.", .5 * w, .92 * h, .6 * w);


                            var t1 = ctx.measureText(scr.leftCropType).width;
                            var t2 = ctx.measureText(" crop.").width;

                            var totalLength = t1 + t2;

                            ctx.fillStyle = "black";
                            ctx.fillText("You must now manage a", .18 * w, .58 * h, .32 * w);

                            ctx.fillStyle = scr.leftCropColor;
                            ctx.fillText(scr.leftCropType, .18 * w - totalLength / 2 + t1 / 2, .62 * h, .32 * w);
                            ctx.fillStyle = "black";
                            ctx.fillText(" crop.", .18 * w - totalLength / 2 + t1 + t2 / 2, .62 * h, .32 * w);


                            var t1 = ctx.measureText("for your  ").width;
                            var t2 = ctx.measureText(scr.rightCropType).width;
                            var t3 = ctx.measureText(" crop.").width;

                            var totalLength = t1 + t2 + t3;

                            ctx.fillStyle = "black";
                            ctx.fillText("Remember, the growing season", .18 * w, .7 * h, .32 * w);

                            ctx.fillText("for your ", .18 * w - totalLength / 2 + t1 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = scr.rightCropColor;
                            ctx.fillText(scr.rightCropType, .18 * w - totalLength / 2 + t1 + t2 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = "black";
                            ctx.fillText(" crop", .18 * w - totalLength / 2 + t1 + t2 + t3 / 2, .74 * h, .32 * w);

                            ctx.fillText("is still in progress.", .18 * w, .78 * h, .32 * w);



                            ctx.font = "20px Times New Roman";
                            ctx.fillText("Press the 'v' key to continue.", .18 * w, .85 * h, .32 * w);

                            scr.deadlinePresented = 0;
                            scr.feedbackPresented = 3;
                            scr.keyNeeded = 'v';
                        }
                    }


                    //If all necessary feedback has been presented, and left crop has weeks remaining
                    if (scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining === 0 && scr.feedbackPresented === 2) {

                        scr.rightWeeksRemaining = scr.fillerWeeksTotal;
                        scr.rightCurrentHeight = scr.fillerStartHeight;

                        //If first filler crop has started
                        if (scr.rightCropType === scr.firstFillerCropType) {
                            scr.rightCropType = scr.secondFillerCropType;
                            scr.rightCropColor = scr.secondFillerCropColor;
                            scr.rightGoalType = 'SecondFiller';
                        }

                        //If first filler crop has not already started
                        if ((scr.rightCropType != scr.firstFillerCropType) && (scr.rightCropType != scr.secondFillerCropType)) {
                            scr.rightCropType = scr.firstFillerCropType;
                            scr.rightCropColor = scr.firstFillerCropColor;
                            scr.rightGoalType = 'FirstFiller';
                        }

                        drawDecisionScreen(canvas, ctx, scr);
                        drawCheckOrEx(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.deadlinePresented = 0;
                        scr.feedbackPresented = 0;
                        scr.keyNeeded = 'response';

                        if (scr.trialType === 'practice') {
                            var w = canvas.width;
                            var h = canvas.height;

                            ctx.clearRect(.01 * w, .47 * h, .34 * w, .52 * h);
                            ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
                            ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);

                            // var t1 = ctx.measureText("In addition to your ").width;
                            // var t2 = ctx.measureText(scr.leftCropType).width;
                            // var t3 = ctx.measureText(" crop, you must now manage a ").width;
                            // var t4 = ctx.measureText(scr.rightCropType).width;
                            // var t5 = ctx.measureText(" crop.").width;
                            // var totalLength = t1 + t2 + t3 + t4 + t5;

                            // ctx.fillStyle = "black";
                            // ctx.fillText("In addition to your ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = scr.leftCropColor;
                            // ctx.fillText(scr.leftCropType, .5 * w - totalLength / 2 + t1 + t2 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = "black";
                            // ctx.fillText(" crop, you must now manage a ", .5 * w - totalLength / 2 + t1 + t2 + t3 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = scr.rightCropColor;
                            // ctx.fillText(scr.rightCropType, .5 * w - totalLength / 2 + t1 + t2 + t3 + t4 / 2, .8 * h, .6 * w);

                            // ctx.fillStyle = "black";
                            // ctx.fillText(" crop.", .5 * w - totalLength / 2 + t1 + t2 + t3 + t4 + t5 / 2, .8 * h, .6 * w);

                            var t1 = ctx.measureText(scr.rightCropType).width;
                            var t2 = ctx.measureText(" crop.").width;

                            var totalLength = t1 + t2;

                            ctx.fillStyle = "black";
                            ctx.fillText("You must now manage a", .82 * w, .58 * h, .32 * w);

                            ctx.fillStyle = scr.rightCropColor;
                            ctx.fillText(scr.rightCropType, .82 * w - totalLength / 2 + t1 / 2, .62 * h, .32 * w);
                            ctx.fillStyle = "black";
                            ctx.fillText(" crop.", .82 * w - totalLength / 2 + t1 + t2 / 2, .62 * h, .32 * w);




                            var t1 = ctx.measureText("for your  ").width;
                            var t2 = ctx.measureText(scr.leftCropType).width;
                            var t3 = ctx.measureText(" crop.").width;

                            var totalLength = t1 + t2 + t3;

                            ctx.fillStyle = "black";
                            ctx.fillText("Remember, the growing season", .82 * w, .7 * h, .32 * w);

                            ctx.fillText("for your ", .82 * w - totalLength / 2 + t1 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = scr.leftCropColor;
                            ctx.fillText(scr.leftCropType, .82 * w - totalLength / 2 + t1 + t2 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = "black";
                            ctx.fillText(" crop", .82 * w - totalLength / 2 + t1 + t2 + t3 / 2, .74 * h, .32 * w);

                            ctx.fillText("is still in progress.", .82 * w, .78 * h, .32 * w);

                            ctx.font = "20px Times New Roman";
                            ctx.fillText("Press the 'v' key to continue.", .82 * w, .85 * h, .32 * w);

                            scr.deadlinePresented = 0;
                            scr.feedbackPresented = 3;
                            scr.keyNeeded = 'v';
                        }
                    }

                    //If left deadline is up, but right is not, and deadline screen has been presented 
                    if (scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining > 0 && scr.deadlinePresented === 1 && scr.feedbackPresented === 0) {
                        feedbackLeft(canvas, ctx, scr);
                        scr.feedbackPresented = 2;
                    }

                    //If right deadline is up, but not left, and deadline screen has been presented 
                    if (scr.leftWeeksRemaining > 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 1 && scr.feedbackPresented === 0) {
                        feedbackRight(canvas, ctx, scr);
                        scr.feedbackPresented = 2;
                    }


                    //If both deadlines are up, and deadline screen has been presented, and both feedback has been presented
                    if (scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 1 && scr.feedbackPresented === 2) {
                        $(document).off('keyup');
                        $(document).off('click');
                        //jsPsych.pluginAPI.cancelAllKeyboardResponses();
                        //Event.stopObserving(document, 'keydown', keydownCallback);
                        //var old_canvas = document.getElementById('canvas');
                        //document.removeChild(old_canvas)
                        //.off( "keydown", display_element, keydownCallback )
                        writeData(scr);
                        jsPsych.finishTrial();
                        // that.clickNextButton();
                    }


                    //If both deadlines are up, and deadline screen has been presented, but only left feedback has been presented
                    if (scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 1 && scr.feedbackPresented === 1) {
                        feedbackRight(canvas, ctx, scr);
                        scr.feedbackPresented = 2;
                    }


                    //If both deadlines are up, and deadline screen has been presented, but no feedback has been presented
                    if (scr.leftWeeksRemaining === 0 && scr.rightWeeksRemaining === 0 && scr.deadlinePresented === 1 && scr.feedbackPresented === 0) {
                        feedbackLeft(canvas, ctx, scr);
                        scr.feedbackPresented = 1;
                    }
                }

                $(document).keydown(function() {
                    keydownCallback(event, canvas, ctx, scr);
                });

            }
            return scr;
        }




        function initializeTrial(trial) {
            //SET BLOCK LEVEL PARAMETERS
            var scr = {
                experimentalGoalHeight: trial.experimental_goal,
                experimentalWeeksTotal: trial.experimental_deadline,
                experimentalStartHeight: trial.experimental_goal - trial.experimental_distance,

                fixedGoalHeight: trial.fixed_goal,
                fixedWeeksTotal: trial.fixed_deadline,
                fixedStartHeight: trial.fixed_goal - trial.fixed_distance,

                nActions: 3,
            };

            //FILLER GOAL PARAMETERS


            //If Experimental and Fixed Deadlines are Equal
            if (scr.experimentalWeeksTotal === scr.fixedWeeksTotal) {
                scr.fillerGoalHeight = NaN;
                scr.fillerWeeksTotal = NaN;
                scr.fillerStartHeight = NaN;
            }

            //If Experimental Deadline is Longer than Fixed Deadline
            if (scr.experimentalWeeksTotal > scr.fixedWeeksTotal) {
                scr.fillerGoalHeight = scr.experimentalGoalHeight;
                scr.fillerWeeksTotal = 10;
                scr.fillerStartHeight = scr.fillerGoalHeight - (scr.fixedGoalHeight - scr.fixedStartHeight) / scr.fixedWeeksTotal * scr.fillerWeeksTotal;
            }

            //If Experimental Deadline is Shorter than Fixed Deadline
            if (scr.experimentalWeeksTotal < scr.fixedWeeksTotal) {
                scr.fillerGoalHeight = scr.experimentalGoalHeight;
                scr.fillerWeeksTotal = 10;
                scr.fillerStartHeight = scr.fillerGoalHeight - (scr.experimentalGoalHeight - scr.experimentalStartHeight) / scr.experimentalWeeksTotal * scr.fillerWeeksTotal;
            }

            //RANDOMLY ASSIGN WHEAT, CORN, RICE, AND BARLEY CROPS TO BE EXPERIMENTAL, FIXED, FILLER 1, FILLER 2    

            var cropTypes = ["WHEAT", "CORN", "RICE", "BARLEY"];
            var shuffledCropTypes = ["WHEAT", "CORN", "RICE", "BARLEY"];
            var cropColors = ["BLUE", "GREEN", "ORANGE", "RED"];

            shuffle(shuffledCropTypes);

            //assign crop types based on shuffled array
            scr.experimentalCropType = shuffledCropTypes[0];
            scr.fixedCropType = shuffledCropTypes[1];
            scr.firstFillerCropType = shuffledCropTypes[2];
            scr.secondFillerCropType = shuffledCropTypes[3];

            scr.experimentalCropColor = cropColors[cropTypes.indexOf(scr.experimentalCropType)];
            scr.fixedCropColor = cropColors[cropTypes.indexOf(scr.fixedCropType)];
            scr.firstFillerCropColor = cropColors[cropTypes.indexOf(scr.firstFillerCropType)];
            scr.secondFillerCropColor = cropColors[cropTypes.indexOf(scr.secondFillerCropType)];

            //RANDOMLY ASSIGN EXPERIMENTAL AND FIXED CROP TO LEFT AND RIGHT SIDE
            var experimentalNum = Math.random();

            //If number is lower than 0.5, Experimental goal on left (and fixed on right)
            //If higher than 0.5, vice versa. Filler takes the same side as fillee. 
            if (experimentalNum > 0.5) {
                scr.experimentalCropSide = 'Left';
            }
            if (experimentalNum <= 0.5) {
                scr.experimentalCropSide = 'Right';
            }

            //SET PARAMETERS FOR TRIAL
            if (scr.experimentalCropSide === 'Left') {
                scr.leftGoalHeight = scr.experimentalGoalHeight;
                scr.leftWeeksTotal = scr.experimentalWeeksTotal;
                scr.leftStartHeight = scr.experimentalStartHeight;
                scr.leftCropType = scr.experimentalCropType;
                scr.leftCropColor = scr.experimentalCropColor;

                scr.rightGoalHeight = scr.fixedGoalHeight;
                scr.rightStartHeight = scr.fixedStartHeight;
                scr.rightWeeksTotal = scr.fixedWeeksTotal;
                scr.rightCropType = scr.fixedCropType;
                scr.rightCropColor = scr.fixedCropColor;

                scr.leftGoalType = 'Experimental';
                scr.rightGoalType = 'Fixed';

                //Determine distribution of growth
                scr.meanLeftGrowthGivenLeft = trial.mean_growth_experimental_if_prioritized;
                scr.sdLeftGrowthGivenLeft = trial.sd_growth_experimental_if_prioritized;
                scr.meanLeftGrowthGivenMiddle = trial.mean_growth_experimental_if_balanced;
                scr.sdLeftGrowthGivenMiddle = trial.sd_growth_experimental_if_balanced;
                scr.meanLeftGrowthGivenRight = trial.mean_growth_experimental_if_not_prioritized;
                scr.sdLeftGrowthGivenRight = trial.sd_growth_experimental_if_not_prioritized;

                //Determine distribution of growth
                scr.meanRightGrowthGivenLeft = trial.mean_growth_fixed_if_not_prioritized;
                scr.sdRightGrowthGivenLeft = trial.sd_growth_fixed_if_not_prioritized;
                scr.meanRightGrowthGivenMiddle = trial.mean_growth_fixed_if_balanced;
                scr.sdRightGrowthGivenMiddle = trial.sd_growth_fixed_if_balanced;
                scr.meanRightGrowthGivenRight = trial.mean_growth_fixed_if_prioritized;
                scr.sdRightGrowthGivenRight = trial.sd_growth_fixed_if_prioritized;
            }

            if (scr.experimentalCropSide === 'Right') {
                scr.leftGoalHeight = scr.fixedGoalHeight;
                scr.leftStartHeight = scr.fixedStartHeight;
                scr.leftWeeksTotal = scr.fixedWeeksTotal;
                scr.leftCropType = scr.fixedCropType;
                scr.leftCropColor = scr.fixedCropColor;

                scr.rightGoalHeight = scr.experimentalGoalHeight;
                scr.rightWeeksTotal = scr.experimentalWeeksTotal;
                scr.rightStartHeight = scr.experimentalStartHeight;
                scr.rightCropType = scr.experimentalCropType;
                scr.rightCropColor = scr.experimentalCropColor;

                scr.leftGoalType = 'Fixed';
                scr.rightGoalType = 'Experimental';

                //Determine distribution of growth
                scr.meanLeftGrowthGivenLeft = trial.mean_growth_fixed_if_prioritized;
                scr.sdLeftGrowthGivenLeft = trial.sd_growth_fixed_if_prioritized;
                scr.meanLeftGrowthGivenMiddle = trial.mean_growth_fixed_if_balanced;
                scr.sdLeftGrowthGivenMiddle = trial.sd_growth_fixed_if_balanced;
                scr.meanLeftGrowthGivenRight = trial.mean_growth_fixed_if_not_prioritized;
                scr.sdLeftGrowthGivenRight = trial.sd_growth_fixed_if_not_prioritized;

                //Determine distribution of growth
                scr.meanRightGrowthGivenLeft = trial.mean_growth_experimental_if_not_prioritized;
                scr.sdRightGrowthGivenLeft = trial.sd_growth_experimental_if_not_prioritized;
                scr.meanRightGrowthGivenMiddle = trial.mean_growth_experimental_if_balanced;
                scr.sdRightGrowthGivenMiddle = trial.sd_growth_experimental_if_balanced;
                scr.meanRightGrowthGivenRight = trial.mean_growth_experimental_if_prioritized;
                scr.sdRightGrowthGivenRight = trial.sd_growth_experimental_if_prioritized;
            }

            if (trial.practice == false) {
                scr.trialType = 'experimental';
            };

            if (trial.practice == true) {
                scr.trialType = 'practice';
            };

            //For screenshots
            //scr.leftCropType = "CORN";
            //scr.leftCropColor = "Green";
            //scr.rightCropType = "BARLEY";
            //scr.rightCropColor = "Red";
            //scr.leftStartHeight = 90;
            //scr.rightStartHeight = 150;
            //scr.leftWeeksTotal = 30;
            //scr.rightWeeksTotal = 10;

            //Initialize Dynamic Variable       
            scr.leftCurrentHeight = scr.leftStartHeight;
            scr.rightCurrentHeight = scr.rightStartHeight;
            scr.leftWeeksRemaining = scr.leftWeeksTotal;
            scr.rightWeeksRemaining = scr.rightWeeksTotal;

            scr.stage = 1;

            scr.trial_data = {
                "experimental_goal_height": scr.experimentalGoalHeight,
                "experimental_start_height": scr.experimentalStartHeight,
                "experimental_weeks_total": scr.experimentalWeeksTotal,
                "experimental_crop_type": scr.experimentalCropType,
                "experimental_crop_side": scr.experimentalCropSide,
                "fixed_goal_height": scr.fixedGoalHeight,
                "fixed_start_height": scr.fixedStartHeight,
                "fixed_weeks_total": scr.fixedWeeksTotal,
                "fixed_crop_type": scr.fixedCropType,
                "trial_kind": scr.trialType,
                "trial_number": trial.trial_number,
            };

            //declare dynamic variables
            scr.trial_data.stage = [];

            scr.trial_data.left_current_height = [];
            scr.trial_data.left_weeks_remaining = [];
            scr.trial_data.left_crop_type = [];
            scr.trial_data.left_goal_type = [];

            scr.trial_data.right_current_height = [];
            scr.trial_data.right_weeks_remaining = [];
            scr.trial_data.right_crop_type = [];
            scr.trial_data.right_goal_type = [];

            scr.trial_data.mean_left_growth_given_left = [];
            scr.trial_data.sd_left_growth_given_left = [];
            scr.trial_data.mean_left_growth_given_middle = [];
            scr.trial_data.sd_left_growth_given_middle = [];
            scr.trial_data.mean_left_growth_given_right = [];
            scr.trial_data.sd_left_growth_given_right = [];

            scr.trial_data.mean_right_growth_given_left = [];
            scr.trial_data.sd_right_growth_given_left = [];
            scr.trial_data.mean_right_growth_given_middle = [];
            scr.trial_data.sd_right_growth_given_middle = [];
            scr.trial_data.mean_right_growth_given_right = [];
            scr.trial_data.sd_right_growth_given_right = [];

            scr.trial_data.response = [];
            scr.trial_data.response_time = [];
            scr.trial_data.left_growth = [];
            scr.trial_data.right_growth = [];

            return scr;


        }

        function drawDecisionScreen(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            drawGoalDetails(canvas, ctx, scr);

            var scaleSpread = .3*w;
            var scaleWidth = .2*w;    

            if (scr.nActions === 2) {
                var scaleCenter = [0.5*w-scaleSpread/2,0.5*w+scaleSpread/2];
                var selectKey = ['s','k'];
            }
            if (scr.nActions === 3) {
                var scaleCenter = [0.5*w-scaleSpread,0.5*w,0.5*w+scaleSpread];
                var selectKey = ['s','g','k'];
            }

            //Draw text above expected growth scales
            ctx.font = "28px Times New Roman Italics";
            ctx.fillStyle = "black";
            
            for (a = 0; a < (scr.nActions); a++) {
                ctx.fillText("Press '" + selectKey[a] + "'", scaleCenter[a], .93 * h, .30 * w);
                //ctx.fillText('AAAA', scaleCenter[a], .9 * h, .30 * w);
            }
        }

        function determineConsequences(scr) {

            //write data
            scr.trial_data.stage.push(scr.stage);

            scr.trial_data.left_current_height.push(scr.leftCurrentHeight);
            scr.trial_data.left_weeks_remaining.push(scr.leftWeeksRemaining);
            scr.trial_data.left_crop_type.push(scr.leftCropType);
            scr.trial_data.left_goal_type.push(scr.leftGoalType);

            scr.trial_data.right_current_height.push(scr.rightCurrentHeight);
            scr.trial_data.right_weeks_remaining.push(scr.rightWeeksRemaining);
            scr.trial_data.right_crop_type.push(scr.rightCropType);
            scr.trial_data.right_goal_type.push(scr.rightGoalType);

            scr.trial_data.mean_left_growth_given_left.push(scr.meanLeftGrowthGivenLeft);
            scr.trial_data.sd_left_growth_given_left.push(scr.sdLeftGrowthGivenLeft);
            scr.trial_data.mean_left_growth_given_middle.push(scr.meanLeftGrowthGivenMiddle);
            scr.trial_data.sd_left_growth_given_middle.push(scr.sdLeftGrowthGivenMiddle);
            scr.trial_data.mean_left_growth_given_right.push(scr.meanLeftGrowthGivenRight);
            scr.trial_data.sd_left_growth_given_right.push(scr.sdLeftGrowthGivenRight);

            scr.trial_data.mean_right_growth_given_left.push(scr.meanRightGrowthGivenLeft);
            scr.trial_data.sd_right_growth_given_left.push(scr.sdRightGrowthGivenLeft);
            scr.trial_data.mean_right_growth_given_middle.push(scr.meanRightGrowthGivenMiddle);
            scr.trial_data.sd_right_growth_given_middle.push(scr.sdRightGrowthGivenMiddle);
            scr.trial_data.mean_right_growth_given_right.push(scr.meanRightGrowthGivenRight);
            scr.trial_data.sd_right_growth_given_right.push(scr.sdRightGrowthGivenRight);



            //Determine Successes 
            if (scr.response === 0) { //left
                scr.leftGrowth = normalRandom() * scr.sdLeftGrowthGivenLeft + scr.meanLeftGrowthGivenLeft;
                scr.rightGrowth = normalRandom() * scr.sdRightGrowthGivenLeft + scr.meanRightGrowthGivenLeft;

            }

            if (scr.response === 1) { //right
                scr.leftGrowth = normalRandom() * scr.sdLeftGrowthGivenRight + scr.meanLeftGrowthGivenRight;
                scr.rightGrowth = normalRandom() * scr.sdRightGrowthGivenRight + scr.meanRightGrowthGivenRight;
            }

            if (scr.response === 2) { //middle
                scr.leftGrowth = normalRandom() * scr.sdLeftGrowthGivenMiddle + scr.meanLeftGrowthGivenMiddle;
                scr.rightGrowth = normalRandom() * scr.sdRightGrowthGivenMiddle + scr.meanRightGrowthGivenMiddle;
            }


            scr.leftCurrentHeight = Math.max(scr.leftCurrentHeight + scr.leftGrowth, 0);
            scr.rightCurrentHeight = Math.max(scr.rightCurrentHeight + scr.rightGrowth, 0);

            //Update Weeks Remaining and Crop Heights               
            scr.leftWeeksRemaining = scr.leftWeeksRemaining - 1;
            scr.rightWeeksRemaining = scr.rightWeeksRemaining - 1;
            scr.stage = scr.stage + 1;

            scr.trial_data.response.push(scr.response);
            scr.trial_data.response_time.push(scr.responseTime);
            scr.trial_data.left_growth.push(scr.leftGrowth);
            scr.trial_data.right_growth.push(scr.rightGrowth);

            return scr;

        }



        function deadlineLeft(canvas, ctx, scr) {
            var w = canvas.width;
            var h = canvas.height;

            drawGoalDetails(canvas, ctx, scr);
            drawCheckOrEx(canvas, ctx, scr);


            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";


            ctx.strokeStyle = "gold";
            ctx.lineWidth = 3;
             ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);
            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
            ctx.strokeRect(.02 * w, .08 * h, .32 * w, .4 * h);

            //ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            //Special function to allow different colors of text in same string
            var t1 = ctx.measureText("The deadline for the ").width;
            var t2 = ctx.measureText(scr.leftCropType).width;
            var t3 = ctx.measureText(" crop goal has been reached.").width;

            // var totalLength = t1 + t2 + t3;

            // ctx.fillText("The deadline for the ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);

            // ctx.fillStyle = scr.leftCropColor;
            // ctx.fillText(scr.leftCropType, .5 * w - totalLength / 2 + t1 + t2 / 2, .8 * h, .6 * w);

            // ctx.fillStyle = "black";
            // ctx.fillText(" crop goal has been reached.", .5 * w - totalLength / 2 + t1 + t2 + t3 / 2, .8 * h, .6 * w);

            // ctx.font = "20px Times New Roman";
            // ctx.fillText("Press the 'v' key to continue.", .5 * w, .92 * h, .6 * w);


            var upperLength = t1 + t2;

            ctx.fillText("The deadline for the ", .18 * w - upperLength / 2 + t1 / 2, .6 * h, .32 * w);

            ctx.fillStyle = scr.leftCropColor;
            ctx.fillText(scr.leftCropType, .18 * w - upperLength / 2 + t1 + t2 / 2, .6 * h, .32 * w);

            ctx.fillStyle = "black";
            ctx.fillText("crop goal has been reached.", .18 * w, .64 * h, .32 * w);

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .18 * w, .7 * h, .32 * w);



        }



        function deadlineRight(canvas, ctx, scr) {
            var w = canvas.width;
            var h = canvas.height;

            drawGoalDetails(canvas, ctx, scr);
            drawCheckOrEx(canvas, ctx, scr);


            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";

            ctx.strokeStyle = "gold";
            ctx.lineWidth = 3;
            ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);
            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
            ctx.strokeRect(.66 * w, .08 * h, .32 * w, .4 * h);
            //ctx.clearRect(.01 * w, .75 * h, .98 * w, .24 * h);
            //ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            //Special function to allow different colors of text in same string
            var t1 = ctx.measureText("The deadline for the ").width;
            var t2 = ctx.measureText(scr.rightCropType).width;
            var t3 = ctx.measureText(" crop goal has been reached.").width;

            //var totalLength = t1 + t2 + t3;

            //ctx.fillText("The deadline for the ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);

            //ctx.fillStyle = scr.rightCropColor;
            //ctx.fillText(scr.rightCropType, .5 * w - totalLength / 2 + t1 + t2 / 2, .8 * h, .6 * w);

            //ctx.fillStyle = "black";
            //ctx.fillText(" crop goal has been reached.", .5 * w - totalLength / 2 + t1 + t2 + t3 / 2, .8 * h, .6 * w);

            //ctx.font = "20px Times New Roman";
            //ctx.fillText("Press the 'v' key to continue.", .5 * w, .92 * h, .6 * w);

            var upperLength = t1 + t2;

            ctx.fillText("The deadline for the ", .82 * w - upperLength / 2 + t1 / 2, .6 * h, .32 * w);

            ctx.fillStyle = scr.rightCropColor;
            ctx.fillText(scr.rightCropType, .82 * w - upperLength / 2 + t1 + t2 / 2, .6 * h, .32 * w);

            ctx.fillStyle = "black";
            ctx.fillText("crop goal has been reached.", .82 * w, .64 * h, .32 * w);

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .82 * w, .7 * h, .32 * w);


        }

        function deadlineBoth(canvas, ctx, scr) {
            var w = canvas.width;
            var h = canvas.height;

            drawGoalDetails(canvas, ctx, scr);
            drawCheckOrEx(canvas, ctx, scr);


            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";


            ctx.strokeStyle = "gold";
            ctx.lineWidth = 3;

            ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);
            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);

            ctx.strokeRect(.02 * w, .08 * h, .32 * w, .4 * h);
            ctx.strokeRect(.66 * w, .08 * h, .32 * w, .4 * h);


            ctx.fillText("The deadlines for BOTH crop", .18 * w, .6 * h, .32 * w);
            ctx.fillText("goals have been reached.", .18 * w, .64 * h, .32 * w);



            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .18 * w, .7 * h, .32 * w);


        }

        function feedbackLeft(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            //Erase the red lines
            ctx.strokeStyle = "white";
            ctx.lineWidth = 5;
            ctx.strokeRect(.02 * w, .08 * h, .32 * w, .4 * h);
            ctx.strokeRect(.66 * w, .08 * h, .32 * w, .4 * h);

            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";

            //Generate feedback 
            ctx.strokeStyle = "gold";
            ctx.lineWidth = 5;

            ctx.clearRect(.01 * w, .47 * h, .34 * w, .52 * h);
            ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
            ctx.strokeRect(.36 * w, .02 * h, .1 * w, .6 * h);
            //ctx.clearRect(.01 * w, .75 * h, .98 * w, .24 * h);
            //ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            scr.leftFinalHeight = scr.leftCurrentHeight;
            if (scr.leftFinalHeight >= scr.leftGoalHeight) {

                //Special function to allow different colors of text in same string
                //var t1 = ctx.measureText("Congratulations! You have achieved your ").width;
                //var t2 = ctx.measureText(scr.leftCropType).width;
                //var t3 = ctx.measureText(" crop goal.").width;

                var t1 = ctx.measureText("your ").width;
                var t2 = ctx.measureText(scr.leftCropType).width;
                var t3 = ctx.measureText(" crop goal.").width;

                var totalLength = t1 + t2 + t3;

                //tx.fillText("Congratulations! You have achieved your ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);c

                ctx.fillText("Congratulations! You have achieved", .18 * w, .6 * h, .34 * w);

                ctx.fillText("your ", .18 * w - totalLength / 2 + t1 / 2, .64 * h, .32 * w);

                ctx.fillStyle = scr.leftCropColor;
                ctx.fillText(scr.leftCropType, .18 * w - totalLength / 2 + t1 + t2 / 2, .64 * h, .32 * w);

                ctx.fillStyle = "black";
                ctx.fillText(" crop goal.", .18 * w - totalLength / 2 + t1 + t2 + t3 / 2, .64 * h, .32 * w);

                scr.leftGoalAchieved = 1;
            }
            if (scr.leftFinalHeight < scr.leftGoalHeight) {

                //Special function to allow different colors of text in same string
                //var t1 = ctx.measureText("Unfortunately, you have not achieved your ").width;
                //var t2 = ctx.measureText(scr.leftCropType).width;
                //var t3 = ctx.measureText(" crop goal.").width;

                var t1 = ctx.measureText("your ").width;
                var t2 = ctx.measureText(scr.leftCropType).width;
                var t3 = ctx.measureText(" crop goal.").width;

                var totalLength = t1 + t2 + t3;

                ctx.fillText("Unfortunately, you have not achieved", .18 * w, .6 * h, .34 * w);

                ctx.fillText("your ", .18 * w - totalLength / 2 + t1 / 2, .64 * h, .32 * w);

                ctx.fillStyle = scr.leftCropColor;
                ctx.fillText(scr.leftCropType, .18 * w - totalLength / 2 + t1 + t2 / 2, .64 * h, .32 * w);

                ctx.fillStyle = "black";
                ctx.fillText(" crop goal.", .18 * w - totalLength / 2 + t1 + t2 + t3 / 2, .64 * h, .32 * w);
                scr.leftGoalAchieved = 0;
            }

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .18 * w, .7 * h, .32 * w);

            return scr;
        }




        function feedbackRight(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            //Erase the red lines
            ctx.strokeStyle = "white";
            ctx.lineWidth = 5;
            ctx.strokeRect(.02 * w, .08 * h, .32 * w, .4 * h);
            ctx.strokeRect(.66 * w, .08 * h, .32 * w, .4 * h);
            ctx.strokeRect(.36 * w, .02 * h, .1 * w, .6 * h);


            ctx.lineWidth = 2;
            ctx.strokeStyle = "black";
            ctx.strokeRect(.36 * w, .02 * h, .1 * w, .6 * h);

            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";

            //Generate feedback 
            ctx.strokeStyle = "gold";
            ctx.lineWidth = 5;
            ctx.clearRect(.01 * w, .47 * h, .34 * w, .52 * h);
            ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
            ctx.strokeRect(.54 * w, .02 * h, .1 * w, .6 * h);
            // ctx.clearRect(.01 * w, .75 * h, .98 * w, .24 * h);
            // ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            scr.rightFinalHeight = scr.rightCurrentHeight;
            if (scr.rightFinalHeight >= scr.rightGoalHeight) {

                //Special function to allow different colors of text in same string
                // var t1 = ctx.measureText("Congratulations! You have achieved your ").width;
                // var t2 = ctx.measureText(scr.rightCropType).width;
                // var t3 = ctx.measureText(" crop goal.").width;

                var t1 = ctx.measureText("your ").width;
                var t2 = ctx.measureText(scr.rightCropType).width;
                var t3 = ctx.measureText(" crop goal.").width;

                var totalLength = t1 + t2 + t3;

                ctx.fillText("Congratulations! You have achieved", .82 * w, .6 * h, .34 * w);

                ctx.fillText("your ", .82 * w - totalLength / 2 + t1 / 2, .64 * h, .32 * w);

                ctx.fillStyle = scr.rightCropColor;
                ctx.fillText(scr.rightCropType, .82 * w - totalLength / 2 + t1 + t2 / 2, .64 * h, .32 * w);

                ctx.fillStyle = "black";
                ctx.fillText(" crop goal.", .82 * w - totalLength / 2 + t1 + t2 + t3 / 2, .64 * h, .32 * w);

                scr.rightGoalAchieved = 1;
            }
            if (scr.rightFinalHeight < scr.rightGoalHeight) {

                //Special function to allow different colors of text in same string
                var t1 = ctx.measureText("your ").width;
                var t2 = ctx.measureText(scr.rightCropType).width;
                var t3 = ctx.measureText(" crop goal.").width;

                var totalLength = t1 + t2 + t3;

                ctx.fillText("Unfortunately, you have not achieved", .82 * w, .6 * h, .34 * w);

                ctx.fillText("your ", .82 * w - totalLength / 2 + t1 / 2, .64 * h, .32 * w);

                ctx.fillStyle = scr.rightCropColor;
                ctx.fillText(scr.rightCropType, .82 * w - totalLength / 2 + t1 + t2 / 2, .64 * h, .32 * w);

                ctx.fillStyle = "black";
                ctx.fillText(" crop goal.", .82 * w - totalLength / 2 + t1 + t2 + t3 / 2, .64 * h, .32 * w);
                scr.rightGoalAchieved = 0;
            }

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .82 * w, .7 * h, .32 * w);

            return scr;
        }


        function drawGoalDetails(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            //Frame entire canvas just to see where it is
            ctx.clearRect(0, 0, w, h);
            ctx.fillStyle = "black";
            ctx.strokeStyle = "black";
            ctx.lineWidth = 2;
            ctx.strokeRect(0, 0, w - 1, h - 1);


             // ctx.fillText(scr.responseTime, 50, 100, 500);
             // ctx.fillText(scr.response, 50, 200, 500);
             // ctx.fillText(scr.keyNeeded, 50, 300, 500);
             // ctx.fillText(scr.trial_data.0other_key_pressed, 50, 400, 500);

            //ctx.fillText(scr.responseTime,50,100);

            //ctx.fillText(scr.secondFillerCropType,100,200,300);

            // Draw Left Crop Details
            ctx.fillStyle = scr.leftCropColor;
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";

            ctx.font = "Bold 56px Times New Roman";
            ctx.fillText(scr.leftCropType + " CROP", .18 * w, .04 * h, .30 * w);

            //ctx.font = "24px Times New Roman Bold";
            //ctx.fillText(scr.leftCropType + " Goal", .18 * w, .12 * h, .30 * w);

            //ctx.font = "32px Times New Roman Bold";
            //ctx.fillText(scr.leftGoalHeight + " cm OR MORE", .18 * w, .15 * h, .30 * w);

            //ctx.font = "24px Times New Roman Bold";
            //ctx.fillText("Current " + scr.leftCropType + " Crop Height", .18 * w, .24 * h, .30 * w);

            //ctx.font = "32px Times New Roman Bold";
            //ctx.fillText(scr.leftCurrentHeight + " cm", .18 * w, .27 * h, .30 * w);

            ctx.font = "24px Times New Roman";
            ctx.fillText("Weeks Left in Growing Season", .18 * w, .11 * h, .30 * w);

            var centerX = .18 * w;
            var centerY = .3 * h;
            var rad = .0001 * w * h;
            var clockMax = 60;
            var clockNiches = 6;

            //Draw clock
            //filed in portion
            ctx.beginPath();
            ctx.moveTo(centerX, centerY);
            ctx.arc(centerX, centerY, rad, 1.5 * Math.PI, ((2 - (scr.leftWeeksRemaining / clockMax) * 2) - 0.5) * Math.PI, true);
            ctx.fill();

            //lines
            for (i = 0; i < clockNiches; i++) {

                var lX = centerX + 1.1 * rad * Math.cos((2 * i / clockNiches - 0.5) * Math.PI);
                var lY = centerY + 1.1 * rad * Math.sin((2 * i / clockNiches - 0.5) * Math.PI);

                ctx.moveTo(centerX, centerY);
                ctx.lineTo(lX, lY);
                ctx.stroke();

                var tX = centerX + 1.25 * rad * Math.cos((2 * i / clockNiches - 0.5) * Math.PI);
                var tY = centerY + 1.25 * rad * Math.sin((2 * i / clockNiches - 0.5) * Math.PI);

                if (i === 0) {
                    var weeks = 0;
                }
                if (i > 0) {
                    var weeks = clockMax - clockMax * i / clockNiches;
                }
                ctx.fillText(weeks, tX, tY, .30 * w);

            }

            ctx.beginPath();
            ctx.arc(centerX, centerY, rad, 0 * Math.PI, 2 * Math.PI);
            ctx.stroke();


            ctx.fillStyle = scr.rightCropColor;

            ctx.font = "Bold 56px Times New Roman";
            ctx.fillText(scr.rightCropType + " CROP", .82 * w, .04 * h, .30 * w);

            // ctx.font = "24px Times New Roman Bold";
            // ctx.fillText(scr.rightCropType + " Goal", .82 * w, .12 * h, .30 * w);

            // ctx.font = "32px Times New Roman Bold";
            // ctx.fillText(scr.rightGoalHeight + " cm OR MORE", .82 * w, .15 * h, .30 * w);

            // ctx.font = "24px Times New Roman Bold";
            // ctx.fillText("Current " + scr.rightCropType + " Crop Height", .82 * w, .24 * h, .30 * w);

            // ctx.font = "32px Times New Roman Bold";
            // ctx.fillText(scr.rightCurrentHeight + " cm", .82 * w, .27 * h, .30 * w);

            ctx.font = "24px Times New Roman";
            ctx.fillText("Weeks Left in Growing Season", .82 * w, .11 * h, .30 * w);

            //Draw clock
            var centerX = .82 * w;
            //filed in portion
            ctx.beginPath();
            ctx.moveTo(centerX, centerY);
            ctx.arc(centerX, centerY, rad, 1.5 * Math.PI, ((2 - (scr.rightWeeksRemaining / clockMax) * 2) - 0.5) * Math.PI, true);
            ctx.fill();

            //lines
            for (i = 0; i < clockNiches; i++) {

                var lX = centerX + 1.1 * rad * Math.cos((2 * i / clockNiches - 0.5) * Math.PI);
                var lY = centerY + 1.1 * rad * Math.sin((2 * i / clockNiches - 0.5) * Math.PI);

                ctx.moveTo(centerX, centerY);
                ctx.lineTo(lX, lY);
                ctx.stroke();

                var tX = centerX + 1.25 * rad * Math.cos((2 * i / clockNiches - 0.5) * Math.PI);
                var tY = centerY + 1.25 * rad * Math.sin((2 * i / clockNiches - 0.5) * Math.PI);

                if (i === 0) {
                    var weeks = 0;
                }
                if (i > 0) {
                    var weeks = clockMax - clockMax * i / clockNiches;
                }

                ctx.fillText(weeks, tX, tY, .30 * w);

            }

            ctx.beginPath();
            ctx.arc(centerX, centerY, rad, 0 * Math.PI, 2 * Math.PI);
            ctx.stroke();

        
            //Append plant images

            var ceiling = 240; //top of scale
            var niches = 12; //number of intervals
            var topOfFrame = 0.02 * h
            var frameHeight = 0.6 * h

            var leftImageTopGap = ((ceiling - scr.leftCurrentHeight) / ceiling) * frameHeight;
            var leftImageHeight = (scr.leftCurrentHeight / ceiling) * frameHeight;
            var rightImageTopGap = ((ceiling - scr.rightCurrentHeight) / ceiling) * frameHeight;
            var rightImageHeight = (scr.rightCurrentHeight / ceiling) * frameHeight;

            if (scr.leftCropType === 'WHEAT') {
                var leftImage = plantImage[0];
                //var leftImage = bluePlantImage;
            }
            if (scr.leftCropType === 'CORN') {
                var leftImage = plantImage[1];
                //var leftImage = greenPlantImage;
            }
            if (scr.leftCropType === 'RICE') {
                var leftImage = plantImage[2];
                //var leftImage = orangePlantImage;
            }
            if (scr.leftCropType === 'BARLEY') {
                var leftImage = plantImage[3];
                //var leftImage = redPlantImage;
            }

            ctx.drawImage(leftImage, .36 * w, topOfFrame + leftImageTopGap, .1 * w, leftImageHeight);

            if (scr.rightCropType === 'WHEAT') {
                var rightImage = plantImage[0];
                //var rightImage = bluePlantImage;
            }
            if (scr.rightCropType === 'CORN') {
                var rightImage = plantImage[1];
                //var rightImage = greenPlantImage;
            }
            if (scr.rightCropType === 'RICE') {
                var rightImage = plantImage[2];
                //var rightImage = orangePlantImage;
            }
            if (scr.rightCropType === 'BARLEY') {
                var rightImage = plantImage[3];
                //var rightImage = redPlantImage;
            }

            ctx.drawImage(rightImage, .54 * w, topOfFrame + rightImageTopGap, .1 * w, rightImageHeight);

            //Frame plant counters  
            ctx.strokeRect(.36 * w, topOfFrame, .1 * w, frameHeight);
            ctx.strokeRect(.54 * w, topOfFrame, .1 * w, frameHeight);

            ctx.font = "20px Times New Roman";
            ctx.fillStyle = "black";

            //Draw niches on the sides of plant counters  
            for (i = 0; i < (niches + 1); i++) {
                var nicheY = (topOfFrame + frameHeight) - frameHeight * i / niches;

                ctx.moveTo(.46 * w, nicheY);
                ctx.lineTo(.47 * w, nicheY);
                ctx.stroke();

                ctx.moveTo(.53 * w, nicheY);
                ctx.lineTo(.54 * w, nicheY);
                ctx.stroke();

                ctx.fillText(i * ceiling / niches, .5 * w, nicheY);
            };

            //Draw goal lines   
            ctx.fillStyle = scr.leftCropColor;
            ctx.fillRect(.36 * w, topOfFrame + ((ceiling - scr.leftGoalHeight) / ceiling) * frameHeight - 0.005 * h, .10 * w, .01 * h);
            ctx.fillStyle = scr.rightCropColor;
            ctx.fillRect(.54 * w, topOfFrame + ((ceiling - scr.rightGoalHeight) / ceiling) * frameHeight - 0.005 * h, .10 * w, .01 * h);


            //Draw Growth Per Week
            var scaleTop = 0.72 * h;
            var scaleHeight = 0.15 * h;
            var scaleAbove0 = 1; //proportion of scale that is below 0

            var scaleSpread = .3*w;
            var scaleWidth = .2*w;    
            var barSpread = .07*w; 
            var barWidth = .035*w;
           

            if (scr.nActions === 2) {
                var scaleCenter = [0.5*w-scaleSpread/2,0.5*w+scaleSpread/2];
                //var selectKey = ['a','l'];
            }
            if (scr.nActions === 3) {
                var scaleCenter = [0.5*w-scaleSpread,0.5*w,0.5*w+scaleSpread];
                //var selectKey = ['a','g','l'];
            }

            //Determine Ceiling and Niches
            var scaleCeiling = 10;
            var unitHeight = scaleHeight / scaleCeiling;
            var nicheInt = 2;
            var nicheHeight = unitHeight * nicheInt;
            var nicheCiel = scaleHeight / unitHeight;
            ctx.lineWidth = 2;

            for (a = 0; a < (scr.nActions); a++) {

                //Left Axes
                ctx.moveTo(scaleCenter[a]-scaleWidth/2, scaleTop + scaleHeight*scaleAbove0);
                ctx.lineTo(scaleCenter[a]+scaleWidth/2, scaleTop + scaleHeight*scaleAbove0);
                ctx.stroke();

                ctx.moveTo(scaleCenter[a]-scaleWidth/2+0.01*w , scaleTop);
                ctx.lineTo(scaleCenter[a]-scaleWidth/2+0.01*w , scaleTop + scaleHeight + .02 * h);
                ctx.stroke();

            //Right Axes
            // ctx.moveTo(0.5*w+scaleSpread/2-scaleWidth/2, scaleTop + scaleHeight*scaleAbove0);
            // ctx.lineTo(0.5*w+scaleSpread/2+scaleWidth/2, scaleTop + scaleHeight*scaleAbove0);
            // ctx.stroke();

            // ctx.moveTo(0.51*w+scaleSpread/2-scaleWidth/2 , scaleTop);
            // ctx.lineTo(0.51*w+scaleSpread/2-scaleWidth/2 , scaleTop + scaleHeight + .02 * h);
            // ctx.stroke();

            

                ctx.lineWidth = 0.5;
                 ctx.font = "24px Times New Roman";
                //Draw major ticks
                for (var i = 0; i < nicheCiel + 1; i += nicheInt) {
                    ctx.fillStyle = "black";
                    ctx.moveTo(scaleCenter[a]-scaleWidth/2, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);
                    ctx.lineTo(scaleCenter[a]-scaleWidth/2+0.02*w, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);
                    ctx.stroke();
                    ctx.fillText( i - nicheCiel*(1-scaleAbove0) , scaleCenter[a]-scaleWidth/2-0.02*w, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);
                }

                //Draw minor ticks

                for (var i = nicheInt / 2; i < nicheCiel + 1; i += nicheInt) {
                    ctx.moveTo(scaleCenter[a]-scaleWidth/2+0.005*w, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);
                    ctx.lineTo(scaleCenter[a]-scaleWidth/2+0.015*w, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);
                    ctx.stroke();
                }

            
            //Draw text above expected growth scales
            ctx.font = "Italic 32px Times New Roman";
            ctx.fillText('Fertiliser ' + (a+1), scaleCenter[a], .68 * h, .30 * w);


                ctx.lineWidth = 2;
                if (a === 0){
                    ctx.fillStyle = scr.leftCropColor;
                    var growthHeight = scaleHeight * (scr.meanLeftGrowthGivenLeft / nicheCiel);
                    ctx.fillRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    ctx.strokeRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                    ctx.fillStyle = scr.rightCropColor;
                    var growthHeight = scaleHeight * (scr.meanRightGrowthGivenLeft / nicheCiel);
                    ctx.fillRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    ctx.strokeRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                } 

                if (a === 1){
                    ctx.fillStyle = scr.leftCropColor;
                    var growthHeight = scaleHeight * (scr.meanLeftGrowthGivenMiddle / nicheCiel);
                    ctx.fillRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    ctx.strokeRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                    ctx.fillStyle = scr.rightCropColor;
                    var growthHeight = scaleHeight * (scr.meanRightGrowthGivenMiddle / nicheCiel);
                    ctx.fillRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    ctx.strokeRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                } 

                if (a === 2){
                    ctx.fillStyle = scr.leftCropColor;
                    var growthHeight = scaleHeight * (scr.meanLeftGrowthGivenRight / nicheCiel);
                    ctx.fillRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    ctx.strokeRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                    ctx.fillStyle = scr.rightCropColor;
                    var growthHeight = scaleHeight * (scr.meanRightGrowthGivenRight / nicheCiel);
                    ctx.fillRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    ctx.strokeRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                } 


                //draw crop labels under bars
                ctx.font = "20px Times New Roman";
                ctx.fillStyle = scr.leftCropColor;
                ctx.fillText(scr.leftCropType, scaleCenter[a]-barSpread/2, scaleTop + scaleHeight + 0.015*h , .30 * w);
                ctx.fillStyle = scr.rightCropColor;
                ctx.fillText(scr.rightCropType, scaleCenter[a]+barSpread/2, scaleTop + scaleHeight + 0.015*h , .30 * w);




            }
            
            
            
      


        }

        function drawCheckOrEx(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            ctx.font = "48px Times New Roman";
            ctx.fillStyle = scr.leftCropColor;

            if (scr.leftCurrentHeight >= scr.leftGoalHeight) {
                ctx.fillText(String.fromCharCode(10003), .41 * w, .06 * h);
            }

            if (scr.leftCurrentHeight < scr.leftGoalHeight) {
                ctx.fillText(String.fromCharCode(10007), .41 * w, .06 * h);
            }

            ctx.fillStyle = scr.rightCropColor;

            if (scr.rightCurrentHeight >= scr.rightGoalHeight) {
                ctx.fillText(String.fromCharCode(10003), .59 * w, .06 * h);
            }

            if (scr.rightCurrentHeight < scr.rightGoalHeight) {
                ctx.fillText(String.fromCharCode(10007), .59 * w, .06 * h);
            }

            ctx.font = "32px Times New Roman";
        }


        function shuffle(array) {
            var currentIndex = array.length,
                temporaryValue, randomIndex;



            // While there remain elements to shuffle...
            while (0 !== currentIndex) {

                // Pick a remaining element...
                randomIndex = Math.floor(Math.random() * currentIndex);
                currentIndex -= 1;

                // And swap it with the current element.
                temporaryValue = array[currentIndex];
                array[currentIndex] = array[randomIndex];
                array[randomIndex] = temporaryValue;
            }



            return array;
        }

        function writeData(scr) {
            jsPsych.data.write(scr.trial_data);
        }

        //Generate random number from standard normal distribution using the Marsaglia polar method
        //var spareRandom = null;
        function normalRandom() {
            var val, u, v, s, mul;

            //if(spareRandom !== null){
            //  val = spareRandom;
            //  spareRandom = null;
            //}
            //else{
            do {
                u = Math.random() * 2 - 1;
                v = Math.random() * 2 - 1;

                s = u * u + v * v;
            } while (s === 0 || s >= 1);

            mul = Math.sqrt(-2 * Math.log(s) / s);

            val = u * mul;
            //spareRandom = v * mul;
            //  }       

            return val;
        }


        //Run Trial
        display_element.html('<canvas height="900" id="canvas" width="1200">Your browser doesn&#39;t support canvas</canvas>');

        var plantImage = new Array();
        plantImage[0] = new Image();
        plantImage[0].src = "js/img/bluePlant.jpg";
        plantImage[1] = new Image();
        plantImage[1].src = "js/img/greenPlant.jpg";
        plantImage[2] = new Image();
        plantImage[2].src = "js/img/orangePlant.jpg";
        plantImage[3] = new Image();
        plantImage[3].src = "js/img/redPlant.jpg";

        var imageCount = plantImage.length;
        var imagesLoaded = 0;

        for (var i = 0; i < imageCount; i++) {
            plantImage[i].onload = function() {
                imagesLoaded++;
                if (imagesLoaded == imageCount) {
                    allLoadedStartTrial();
                }
            }
        }

        function allLoadedStartTrial() {


            var canvas = document.getElementById("canvas");
            var ctx = canvas.getContext("2d");

            scr = initializeTrial(trial);

            drawDecisionScreen(canvas, ctx, scr);

            //ctx.fillText(imageCount, 100, 100, 100);


            scr.preTrialScreen = 0;
            scr.trial_data.other_key_pressed = 0;
            scr.trial_data.mouse_clicked = 0;
            
            if (scr.trialType == 'experimental'){
                scr.keyNeeded = 'response';
            };

            if (scr.trialType == 'practice') {

                var w = canvas.width;
                var h = canvas.height;
                ctx.fillStyle = "white";
                ctx.clearRect(.01 * w, .1 * h, .34 * w, .89 * h);
                ctx.clearRect(.65 * w, .1 * h, .34 * w, .89 * h);
                ctx.clearRect(.01 * w, .65 * h, .98 * w, .34 * h);
                //ctx.clearRect(.3 * w, .73 * h, .4 * w, .26 * h);

                ctx.fillStyle = "brown";
                ctx.lineWidth = 3;
                ctx.font = "36px Times New Roman";
                ctx.fillText("On this trial, you must manage", .18 * w, .4 * h, .32 * w);
                ctx.fillText(scr.leftCropType + " and " + scr.rightCropType + " crops.", .18 * w, .44 * h, .32 * w);

                ctx.font = "20px Times New Roman";
                ctx.fillText("Press the 'v' key to continue.", .18 * w, .5 * h, .32 * w);

                //ctx.fillText("Congratulations! You have achieved", .18 * w, .6 * h, .32 * w);

                scr.preTrialScreen = 1;
                scr.keyNeeded = 'v';

                //ctx.fillText(scr.keyNeeded, 100, 100, 100);

            };


            scr.deadlinePresented = 0;
            scr.feedbackPresented = 0;


            var that = this;
            scr.start = new Date().getTime();


            //For running the experiment
            $(document).keydown(function() {
                keydownCallback(event, canvas, ctx, scr);
            });

            $(document).click(function() {
                scr.trial_data.mouse_clicked = scr.trial_data.mouse_clicked + 1;
            });



            // $(document).keydown(function() {
            //     keydownCallback(event, canvas, ctx, scr);
            // });


            //For simulating the experiment
            // var info = {
            //     keyCode: [],
            //     }

            // for(var i = 0; i < 70; i++){
            // setTimeout(function (){
            //     r = Math.random();

            //     if (r < 0.5){
            //         info.keyCode = 65;
            //         keydownCallback(info,canvas,ctx,scr);
            //     }

            //     if (r >= 0.5){
            //         info.keyCode = 75;
            //         keydownCallback(info,canvas,ctx,scr);
            //     }    

            //     info.keyCode = 86;
            //     keydownCallback(info,canvas,ctx,scr);
            // }, 100);
            // }

        }
    };

    return plugin;

})();