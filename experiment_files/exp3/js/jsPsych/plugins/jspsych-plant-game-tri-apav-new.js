//**
// * jsPsych plugin for running Tim's plant task
jsPsych['plant-game'] = (function() {

    var plugin = {};

    plugin.create = function(params) {

        //ADD MEAN AND SD GROWTH TO THIS
        params = jsPsych.pluginAPI.enforceArray(params, ['goal_type','experimental_deadline', 'experimental_distance', 'experimental_goal', 'fixed_deadline', 'fixed_distance', 'fixed_goal',
            'mean_growth_experimental_if_prioritized', 'sd_growth_experimental_if_prioritized', 'mean_growth_experimental_if_not_prioritized', 'sd_growth_experimental_if_not_prioritized',
            'mean_growth_fixed_if_prioritized', 'sd_growth_fixed_if_prioritized', 'mean_growth_fixed_if_not_prioritized', 'sd_growth_fixed_if_not_prioritized'
        ]);

        var trials = [];

        trials[0] = {};
        //Note we have to do a mathematical operation on them to convert the object from an array to a plain number object. This simplifies the data structure.
        trials[0].goal_type = params.goal_type * 1;

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
                        //drawExclamationMark(canvas, ctx, scr);
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
                        //drawExclamationMark(canvas, ctx, scr);
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
                        //drawExclamationMark(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        //ctx.fillText(scr.responseTime,100,100,100);
                    }

                }

                if (keyupevent.keyCode === 86) {

                    //Begin Trial
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 4) {
                        scr.keyNeeded = 'response';
                        drawDecisionScreen(canvas, ctx, scr);
                        //drawExclamationMark(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.preTrialScreen = 0;
                        scr.keyNeeded = 'response';


                    }


                    //Practice Trial Screen 5
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 3) {
                        var w = canvas.width;
                        var h = canvas.height;
                        ctx.fillStyle = "black";
                        ctx.clearRect(.005 * w, .01 * h, .99 * w, .98 * h);
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

                        ctx.strokeStyle = "blue";
                        ctx.strokeRect(.01 * w, .64 * h, .98 * w, .32 * h);
                        //ctx.strokeRect(.65 * w, .47 * h, .34 * w, .37 * h);

                        ctx.fillStyle = "blue";
                        ctx.lineWidth = 3;
                        ctx.font = "28px Times New Roman";
                        ctx.fillText("Here are the growth estimates", .82 * w, .52 * h, .32 * w);
                        ctx.fillText("for each treatment.", .82 * w, .56 * h, .32 * w);
                        ctx.font = "20px Times New Roman";
                        ctx.fillText("Press the 'v' key to continue.", .82 * w, .62 * h, .3 * w);


                        scr.preTrialScreen = 3;

                    }

                    //Practice Trial Screen 1
                    if (scr.trialType === 'practice' && scr.preTrialScreen === 1) {
                        drawDecisionScreen(canvas, ctx, scr);

                        var w = canvas.width;
                        var h = canvas.height;
                        ctx.fillStyle = "white";
                        ctx.clearRect(.01 * w, .49 * h, .34 * w, .5 * h);
                        ctx.clearRect(.65 * w, .49 * h, .34 * w, .5 * h);
                        ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
                        //ctx.clearRect(.25 * w, .73 * h, .5 * w, .26 * h);

                        ctx.strokeStyle = "blue";
                        ctx.strokeRect(.02 * w, .1 * h, .32 * w, .4 * h);
                        ctx.strokeRect(.66 * w, .1 * h, .32 * w, .4 * h);

                        ctx.fillStyle = "blue";
                        ctx.lineWidth = 3;
                        ctx.font = "28px Times New Roman";
                        ctx.fillText("Here are the number of months", .18 * w, .53 * h, .32 * w);
                        ctx.fillText("in each growing season.", .18 * w, .57 * h, .32 * w);
                        ctx.font = "20px Times New Roman";
                        ctx.fillText("Press the 'v' key to continue.", .18 * w, .61 * h, .3 * w);

                        scr.preTrialScreen = 2;
                    }

                    //If fillers have just been introduced (which happens in the practice trial), draw decision screen and reset deadline and feedback screens
                    if (scr.feedbackPresented === 3) {
                        drawDecisionScreen(canvas, ctx, scr);
                        //drawExclamationMark(canvas, ctx, scr);
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
                            scr.leftCropTypeShort = scr.secondFillerCropTypeShort;
                            scr.leftCropColor = scr.secondFillerCropColor;
                            scr.leftGoalType = 'SecondFiller';
                        }

                        //If first filler crop has not already started
                        if ((scr.leftCropType != scr.firstFillerCropType) && (scr.leftCropType != scr.secondFillerCropType)) {
                            scr.leftCropType = scr.firstFillerCropType;
                            scr.leftCropTypeShort = scr.firstFillerCropTypeShort;
                            scr.leftCropColor = scr.firstFillerCropColor;
                            scr.leftGoalType = 'FirstFiller';
                        }

                        drawDecisionScreen(canvas, ctx, scr);
                        //drawExclamationMark(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.deadlinePresented = 0;
                        scr.feedbackPresented = 0;
                        scr.keyNeeded = 'response';

                        if (scr.trialType === 'practice') {
                            var w = canvas.width;
                            var h = canvas.height;

                            ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
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
                            var t2 = ctx.measureText(".").width;

                            var totalLength = t1 + t2;

                            ctx.fillStyle = "black";
                            ctx.fillText("You must now manage a", .18 * w, .58 * h, .32 * w);

                            ctx.fillStyle = scr.leftCropColor;
                            ctx.fillText(scr.leftCropType, .18 * w - totalLength / 2 + t1 / 2, .62 * h, .32 * w);
                            ctx.fillStyle = "black";
                            ctx.fillText(".", .18 * w - totalLength / 2 + t1 + t2 / 2, .62 * h, .32 * w);


                            var t1 = ctx.measureText("for your  ").width;
                            var t2 = ctx.measureText(scr.rightCropType).width;
                            var t3 = ctx.measureText(".").width;

                            var totalLength = t1 + t2 + t3;

                            ctx.fillStyle = "black";
                            ctx.fillText("Remember, the growing season", .18 * w, .7 * h, .32 * w);

                            ctx.fillText("for your ", .18 * w - totalLength / 2 + t1 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = scr.rightCropColor;
                            ctx.fillText(scr.rightCropType, .18 * w - totalLength / 2 + t1 + t2 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = "black";
                            ctx.fillText("", .18 * w - totalLength / 2 + t1 + t2 + t3 / 2, .74 * h, .32 * w);

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
                            scr.rightCropTypeShort = scr.secondFillerCropTypeShort;
                            scr.rightCropColor = scr.secondFillerCropColor;
                            scr.rightGoalType = 'SecondFiller';
                        }

                        //If first filler crop has not already started
                        if ((scr.rightCropType != scr.firstFillerCropType) && (scr.rightCropType != scr.secondFillerCropType)) {
                            scr.rightCropType = scr.firstFillerCropType;
                            scr.rightCropTypeShort = scr.firstFillerCropTypeShort;
                            scr.rightCropColor = scr.firstFillerCropColor;
                            scr.rightGoalType = 'FirstFiller';
                        }

                        drawDecisionScreen(canvas, ctx, scr);
                        //drawExclamationMark(canvas, ctx, scr);
                        scr.start = new Date().getTime();
                        scr.deadlinePresented = 0;
                        scr.feedbackPresented = 0;
                        scr.keyNeeded = 'response';

                        if (scr.trialType === 'practice') {
                            var w = canvas.width;
                            var h = canvas.height;

                            ctx.clearRect(.01 * w, .49 * h, .34 * w, .50 * h);
                            ctx.clearRect(.65 * w, .49 * h, .34 * w, .50 * h);
                            ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);

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
                            var t2 = ctx.measureText(".").width;

                            var totalLength = t1 + t2;

                            ctx.fillStyle = "black";
                            ctx.fillText("You must now manage a", .82 * w, .58 * h, .32 * w);

                            ctx.fillStyle = scr.rightCropColor;
                            ctx.fillText(scr.rightCropType, .82 * w - totalLength / 2 + t1 / 2, .62 * h, .32 * w);
                            ctx.fillStyle = "black";
                            ctx.fillText(".", .82 * w - totalLength / 2 + t1 + t2 / 2, .62 * h, .32 * w);




                            var t1 = ctx.measureText("for your  ").width;
                            var t2 = ctx.measureText(scr.leftCropType).width;
                            var t3 = ctx.measureText("").width;

                            var totalLength = t1 + t2 + t3;

                            ctx.fillStyle = "black";
                            ctx.fillText("Remember, the growing season", .82 * w, .7 * h, .32 * w);

                            ctx.fillText("for your ", .82 * w - totalLength / 2 + t1 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = scr.leftCropColor;
                            ctx.fillText(scr.leftCropType, .82 * w - totalLength / 2 + t1 + t2 / 2, .74 * h, .32 * w);

                            ctx.fillStyle = "black";
                            ctx.fillText("", .82 * w - totalLength / 2 + t1 + t2 + t3 / 2, .74 * h, .32 * w);

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
                goalType: trial.goal_type,

                experimentalGoalHeight: trial.experimental_goal,
                experimentalWeeksTotal: trial.experimental_deadline,
                experimentalStartHeight: trial.experimental_goal - trial.experimental_distance,

                fixedGoalHeight: trial.fixed_goal,
                fixedWeeksTotal: trial.fixed_deadline,
                fixedStartHeight: trial.fixed_goal - trial.fixed_distance,

                nActions: 2,
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
                scr.fillerWeeksTotal = scr.experimentalWeeksTotal - scr.fixedWeeksTotal ;
                scr.fillerStartHeight = scr.fillerGoalHeight - 10 * (scr.experimentalWeeksTotal-scr.fixedWeeksTotal);
            }

            //If Experimental Deadline is Shorter than Fixed Deadline
            if (scr.experimentalWeeksTotal < scr.fixedWeeksTotal) {
                scr.fillerGoalHeight = scr.experimentalGoalHeight;
                scr.fillerWeeksTotal = scr.fixedWeeksTotal - scr.experimentalWeeksTotal;
                scr.fillerStartHeight = scr.fillerGoalHeight - 10 * (scr.fixedWeeksTotal-scr.experimentalWeeksTotal);         }

            //RANDOMLY ASSIGN WHEAT, CORN, RICE, AND BARLEY CROPS TO BE EXPERIMENTAL, FIXED, FILLER 1, FILLER 2    
            if ( scr.goalType == 1   ) {
                var cropTypes = ["WHEAT CROP", "CORN CROP", "RICE CROP"];
                var cropTypesShort = ["WHEAT", "CORN", "RICE"];
                var shuffledCropTypes = ["WHEAT CROP", "CORN CROP", "RICE CROP"];
                var cropColors = ["DARKGREEN", "YELLOWGREEN", "LIGHTSEAGREEN"];
            }

            if ( scr.goalType == 2   ) {
                var cropTypes = ["THISTLE WEED", "NETTLE WEED", "LANTANA WEED"];
                var cropTypesShort = ["THISTLE", "NETTLE", "LANTANA"];
                var shuffledCropTypes = ["THISTLE WEED", "NETTLE WEED", "LANTANA WEED"];
                var cropColors = ["SADDLEBROWN","GOLDENROD","CORAL"];
            }

        
            shuffle(shuffledCropTypes);

            //assign crop types based on shuffled array
            scr.experimentalCropType = shuffledCropTypes[0];
            scr.fixedCropType = shuffledCropTypes[1];
            scr.firstFillerCropType = shuffledCropTypes[2];
            scr.secondFillerCropType = NaN;

            scr.experimentalCropTypeShort = cropTypesShort[cropTypes.indexOf(scr.experimentalCropType)];
            scr.fixedCropTypeShort = cropTypesShort[cropTypes.indexOf(scr.fixedCropType)];
            scr.firstFillerCropTypeShort = cropTypesShort[cropTypes.indexOf(scr.firstFillerCropType)];
            scr.secondFillerCropTypeShort = NaN;

            scr.experimentalCropColor = cropColors[cropTypes.indexOf(scr.experimentalCropType)];
            scr.fixedCropColor = cropColors[cropTypes.indexOf(scr.fixedCropType)];
            scr.firstFillerCropColor = cropColors[cropTypes.indexOf(scr.firstFillerCropType)];
            scr.secondFillerCropColor = NaN;

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
                scr.leftCropTypeShort = scr.experimentalCropTypeShort;
                scr.leftCropColor = scr.experimentalCropColor;

                scr.rightGoalHeight = scr.fixedGoalHeight;
                scr.rightStartHeight = scr.fixedStartHeight;
                scr.rightWeeksTotal = scr.fixedWeeksTotal;
                scr.rightCropType = scr.fixedCropType;
                scr.rightCropTypeShort = scr.fixedCropTypeShort;
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
                scr.leftCropTypeShort = scr.fixedCropTypeShort;
                scr.leftCropColor = scr.fixedCropColor;

                scr.rightGoalHeight = scr.experimentalGoalHeight;
                scr.rightWeeksTotal = scr.experimentalWeeksTotal;
                scr.rightStartHeight = scr.experimentalStartHeight;
                scr.rightCropType = scr.experimentalCropType;
                scr.rightCropTypeShort = scr.experimentalCropTypeShort;
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

            //For screenshot 1
            // scr.leftCropType = "NETTLE WEED";
            // scr.leftCropColor = "GOLDENROD";
            // scr.rightCropType = "THISTLE WEED";
            // scr.rightCropColor = "SADDLEBROWN";
            // scr.leftStartHeight = 160;
            // scr.rightStartHeight = 180;
            // scr.leftWeeksTotal = 4;
            // scr.rightWeeksTotal = 2;
            // scr.goalType = 2;

            // //For screenshot 2
            //  scr.leftCropType = "NETTLE WEED";
            // scr.leftCropColor = "GOLDENROD";
            // scr.rightCropType = "THISTLE WEED";
            // scr.rightCropColor = "SADDLEBROWN";
            // scr.leftStartHeight = 160;
            // scr.rightStartHeight = 180;
            // scr.leftWeeksTotal = 4;
            // scr.rightWeeksTotal = 2;
            // scr.goalType = 2;

            // //For screenshot 3
            // scr.leftCropType = "CORN CROP";
            // scr.leftCropColor = "YELLOWGREEN";
            // scr.rightCropType = "RICE CROP";
            // scr.rightCropColor = "LIGHTSEAGREEN";
            // scr.leftStartHeight = 160;
            // scr.rightStartHeight = 180;
            // scr.leftWeeksTotal = 4;
            // scr.rightWeeksTotal = 2;
            // scr.goalType = 1;

            // //For screenshot 3
            // scr.leftCropType = "CORN CROP";
            // scr.leftCropColor = "YELLOWGREEN";
            // scr.rightCropType = "RICE CROP";
            // scr.rightCropColor = "LIGHTSEAGREEN";
            // scr.leftStartHeight = 160;
            // scr.rightStartHeight = 180;
            // scr.leftWeeksTotal = 4;
            // scr.rightWeeksTotal = 2;
            // scr.goalType = 1;



            //Initialize Dynamic Variable       
            scr.leftCurrentHeight = scr.leftStartHeight;
            scr.rightCurrentHeight = scr.rightStartHeight;
            scr.leftWeeksRemaining = scr.leftWeeksTotal;
            scr.rightWeeksRemaining = scr.rightWeeksTotal;

            scr.stage = 1;

            scr.trial_data = {
                "goal_type": scr.goalType,
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
                ctx.fillText("Press '" + selectKey[a] + "'", scaleCenter[a], .97 * h, .30 * w);
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

            //Play sounds if goal was achieved on last turn
            if (scr.goalType === 1){
                if( ((scr.leftCurrentHeight - scr.leftGrowth) < scr.leftGoalHeight ) && scr.leftCurrentHeight >= scr.leftGoalHeight ){
                    audioApproach.play();
                }
                if( ((scr.rightCurrentHeight - scr.rightGrowth) < scr.rightGoalHeight ) && scr.rightCurrentHeight >= scr.rightGoalHeight ){
                    audioApproach.play();
                }
            }
            if (scr.goalType === 2){
                if( ((scr.leftCurrentHeight - scr.leftGrowth) < scr.leftGoalHeight ) && scr.leftCurrentHeight >= scr.leftGoalHeight ){
                    audioAvoidance.play();
                }
                if( ((scr.rightCurrentHeight - scr.rightGrowth) < scr.rightGoalHeight ) && scr.rightCurrentHeight >= scr.rightGoalHeight ){
                    audioAvoidance.play();
                }
            }
           
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
            //drawExclamationMark(canvas, ctx, scr);


            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";


            ctx.strokeStyle = "blue";
            ctx.lineWidth = 3;
             ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
            ctx.strokeRect(.02 * w, .1 * h, .32 * w, .4 * h);

            //ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            //Special function to allow different colors of text in same string
            var t1 = ctx.measureText("The growing season for the ").width;
            var t2 = ctx.measureText(scr.leftCropType).width;
            var t3 = ctx.measureText(" is over.").width;

            // var totalLength = t1 + t2 + t3;

            // ctx.fillText("The deadline for the ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);

            // ctx.fillStyle = scr.leftCropColor;
            // ctx.fillText(scr.leftCropType, .5 * w - totalLength / 2 + t1 + t2 / 2, .8 * h, .6 * w);

            // ctx.fillStyle = "black";
            // ctx.fillText(" crop goal has been reached.", .5 * w - totalLength / 2 + t1 + t2 + t3 / 2, .8 * h, .6 * w);

            // ctx.font = "20px Times New Roman";
            // ctx.fillText("Press the 'v' key to continue.", .5 * w, .92 * h, .6 * w);


            var upperLength = t1 + t2;

            ctx.fillText("The growing season for the ", .25 * w - upperLength / 2 + t1 / 2, .75 * h, .32 * w);

            ctx.fillStyle = scr.leftCropColor;
            ctx.fillText(scr.leftCropType, .25 * w - upperLength / 2 + t1 + t2 / 2, .75 * h, .32 * w);

            ctx.fillStyle = "black";
            ctx.fillText(" is over.", .25 * w, .79 * h, .32 * w);

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .25 * w, .85 * h, .32 * w);



        }



        function deadlineRight(canvas, ctx, scr) {
            var w = canvas.width;
            var h = canvas.height;

            drawGoalDetails(canvas, ctx, scr);
            //drawExclamationMark(canvas, ctx, scr);


            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";

            ctx.strokeStyle = "blue";
            ctx.lineWidth = 3;
            ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);
            ctx.strokeRect(.66 * w, .1 * h, .32 * w, .4 * h);
            //ctx.clearRect(.01 * w, .75 * h, .98 * w, .24 * h);
            //ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            //Special function to allow different colors of text in same string
            var t1 = ctx.measureText("The growing season for the ").width;
            var t2 = ctx.measureText(scr.rightCropType).width;
            var t3 = ctx.measureText(" is over.").width;

            //var totalLength = t1 + t2 + t3;

            //ctx.fillText("The deadline for the ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);

            //ctx.fillStyle = scr.rightCropColor;
            //ctx.fillText(scr.rightCropType, .5 * w - totalLength / 2 + t1 + t2 / 2, .8 * h, .6 * w);

            //ctx.fillStyle = "black";
            //ctx.fillText(" crop goal has been reached.", .5 * w - totalLength / 2 + t1 + t2 + t3 / 2, .8 * h, .6 * w);

            //ctx.font = "20px Times New Roman";
            //ctx.fillText("Press the 'v' key to continue.", .5 * w, .92 * h, .6 * w);

            var upperLength = t1 + t2;

            ctx.fillText("The growing season for the ", .75 * w - upperLength / 2 + t1 / 2, .75 * h, .32 * w);

            ctx.fillStyle = scr.rightCropColor;
            ctx.fillText(scr.rightCropType, .75 * w - upperLength / 2 + t1 + t2 / 2, .75 * h, .32 * w);

            ctx.fillStyle = "black";
            ctx.fillText(" is over.", .75 * w, .79 * h, .32 * w);

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .75 * w, .85 * h, .32 * w);


        }

        function deadlineBoth(canvas, ctx, scr) {
            var w = canvas.width;
            var h = canvas.height;

            drawGoalDetails(canvas, ctx, scr);
            //drawExclamationMark(canvas, ctx, scr);


            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";


            ctx.strokeStyle = "blue";
            ctx.lineWidth = 3;

            ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
            //ctx.clearRect(.65 * w, .47 * h, .34 * w, .52 * h);

            ctx.strokeRect(.02 * w, .1 * h, .32 * w, .4 * h);
            ctx.strokeRect(.66 * w, .1 * h, .32 * w, .4 * h);


            ctx.fillText("Both growing seasons are over.", .5 * w, .77 * h, .32 * w);
            //ctx.fillText("are over.", .5 * w, .79 * h, .32 * w);



            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .5 * w, .85 * h, .32 * w);


        }

        function feedbackLeft(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            //Erase the red lines
            ctx.strokeStyle = "white";
            ctx.lineWidth = 5;
            ctx.strokeRect(.02 * w, .1 * h, .32 * w, .4 * h);
            ctx.strokeRect(.66 * w, .1 * h, .32 * w, .4 * h);

            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";

            //Generate feedback 
            ctx.strokeStyle = "blue";
            ctx.lineWidth = 5;

            ctx.clearRect(.01 * w, .49 * h, .34 * w, .5 * h);
            ctx.clearRect(.65 * w, .49 * h, .34 * w, .5 * h);
            ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
            ctx.strokeRect(.36 * w, .02 * h, .1 * w, .6 * h);
            //ctx.clearRect(.01 * w, .75 * h, .98 * w, .24 * h);
            //ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            scr.leftFinalHeight = scr.leftCurrentHeight;
            if (scr.goalType === 1){
                if (scr.leftFinalHeight >= scr.leftGoalHeight) {

                    //Special function to allow different colors of text in same string
                    //var t1 = ctx.measureText("Congratulations! You have achieved your ").width;
                    //var t2 = ctx.measureText(scr.leftCropType).width;
                    //var t3 = ctx.measureText(" crop goal.").width;

                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.leftCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    //tx.fillText("Congratulations! You have achieved your ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);c

                    ctx.fillText("Congratulations! You have achieved", .25 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .25 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.leftCropColor;
                    ctx.fillText(scr.leftCropType, .25 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .25 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);

                    scr.leftGoalAchieved = 1;
                }
                if (scr.leftFinalHeight < scr.leftGoalHeight) {

                    //Special function to allow different colors of text in same string
                    //var t1 = ctx.measureText("Unfortunately, you have not achieved your ").width;
                    //var t2 = ctx.measureText(scr.leftCropType).width;
                    //var t3 = ctx.measureText(" crop goal.").width;

                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.leftCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    ctx.fillText("Unfortunately, you have not achieved", .25 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .25 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.leftCropColor;
                    ctx.fillText(scr.leftCropType, .25 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .25 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);
                    scr.leftGoalAchieved = 0;
                }
            }   

            if (scr.goalType === 2){
                if (scr.leftFinalHeight <= scr.leftGoalHeight) {

                    //Special function to allow different colors of text in same string
                    //var t1 = ctx.measureText("Congratulations! You have achieved your ").width;
                    //var t2 = ctx.measureText(scr.leftCropType).width;
                    //var t3 = ctx.measureText(" crop goal.").width;

                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.leftCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    //tx.fillText("Congratulations! You have achieved your ", .5 * w - totalLength / 2 + t1 / 2, .8 * h, .6 * w);c

                    ctx.fillText("Congratulations! You have achieved", .25 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .25 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.leftCropColor;
                    ctx.fillText(scr.leftCropType, .25 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .25 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);

                    scr.leftGoalAchieved = 1;
                }
                if (scr.leftFinalHeight > scr.leftGoalHeight) {

                    //Special function to allow different colors of text in same string
                    //var t1 = ctx.measureText("Unfortunately, you have not achieved your ").width;
                    //var t2 = ctx.measureText(scr.leftCropType).width;
                    //var t3 = ctx.measureText(" crop goal.").width;

                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.leftCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    ctx.fillText("Unfortunately, you have not achieved", .25 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .25 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.leftCropColor;
                    ctx.fillText(scr.leftCropType, .25 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .25 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);
                    scr.leftGoalAchieved = 0;
                }
            }      

            ctx.font = "20px Times New Roman";
            ctx.fillText("Press the 'v' key to continue.", .25 * w, .85 * h, .32 * w);

            return scr;
        }




        function feedbackRight(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            //Erase the red lines
            ctx.strokeStyle = "white";
            ctx.lineWidth = 6;
            ctx.strokeRect(.02 * w, .1 * h, .32 * w, .4 * h);
            ctx.strokeRect(.66 * w, .1 * h, .32 * w, .4 * h);
            ctx.strokeRect(.36 * w, .02 * h, .1 * w, .6 * h);


            ctx.lineWidth = 2;
            ctx.strokeStyle = "black";
            ctx.strokeRect(.36 * w, .02 * h, .1 * w, .6 * h);

            ctx.font = "Italic 32px Times New Roman";
            ctx.fillStyle = "black";

            //Generate feedback 
            ctx.strokeStyle = "blue";
            ctx.lineWidth = 5;
            ctx.clearRect(.01 * w, .49 * h, .34 * w, .5 * h);
            ctx.clearRect(.65 * w, .49 * h, .34 * w, .5 * h);
            ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
            ctx.strokeRect(.54 * w, .02 * h, .1 * w, .6 * h);
            // ctx.clearRect(.01 * w, .75 * h, .98 * w, .24 * h);
            // ctx.clearRect(.4 * w, .73 * h, .2 * w, .02 * h);

            scr.rightFinalHeight = scr.rightCurrentHeight;
            if(scr.goalType === 1){
                if (scr.rightFinalHeight >= scr.rightGoalHeight) {

                    //Special function to allow different colors of text in same string
                    // var t1 = ctx.measureText("Congratulations! You have achieved your ").width;
                    // var t2 = ctx.measureText(scr.rightCropType).width;
                    // var t3 = ctx.measureText(" crop goal.").width;

                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.rightCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    ctx.fillText("Congratulations! You have achieved", .75 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .75 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.fillText(scr.rightCropType, .75 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .75 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);

                    scr.rightGoalAchieved = 1;
                }
                if (scr.rightFinalHeight < scr.rightGoalHeight) {

                    //Special function to allow different colors of text in same string
                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.rightCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    ctx.fillText("Unfortunately, you have not achieved", .75 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .75 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.fillText(scr.rightCropType, .75 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .75 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);
                    scr.rightGoalAchieved = 0;
                }
            }

            if(scr.goalType === 2){
                if (scr.rightFinalHeight <= scr.rightGoalHeight) {

                    //Special function to allow different colors of text in same string
                    // var t1 = ctx.measureText("Congratulations! You have achieved your ").width;
                    // var t2 = ctx.measureText(scr.rightCropType).width;
                    // var t3 = ctx.measureText(" crop goal.").width;

                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.rightCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    ctx.fillText("Congratulations! You have achieved", .75 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .75 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.fillText(scr.rightCropType, .75 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .75 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);

                    scr.rightGoalAchieved = 1;
                }
                if (scr.rightFinalHeight > scr.rightGoalHeight) {

                    //Special function to allow different colors of text in same string
                    var t1 = ctx.measureText("your ").width;
                    var t2 = ctx.measureText(scr.rightCropType).width;
                    var t3 = ctx.measureText(" goal.").width;

                    var totalLength = t1 + t2 + t3;

                    ctx.fillText("Unfortunately, you have not achieved", .75 * w, .75 * h, .34 * w);

                    ctx.fillText("your ", .75 * w - totalLength / 2 + t1 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.fillText(scr.rightCropType, .75 * w - totalLength / 2 + t1 + t2 / 2, .79 * h, .32 * w);

                    ctx.fillStyle = "black";
                    ctx.fillText(" goal.", .75 * w - totalLength / 2 + t1 + t2 + t3 / 2, .79 * h, .32 * w);
                    scr.rightGoalAchieved = 0;
                }
            }

                ctx.font = "20px Times New Roman";
                ctx.fillText("Press the 'v' key to continue.", .75 * w, .85 * h, .32 * w);

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


             //ctx.fillText(scr.trial_data.age, 50, 100, 500);
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
            ctx.fillText(scr.leftCropType, .18 * w, .04 * h, .30 * w);

            //ctx.font = "24px Times New Roman Bold";
            //ctx.fillText(scr.leftCropType + " Goal", .18 * w, .12 * h, .30 * w);

            //ctx.font = "32px Times New Roman Bold";
            //ctx.fillText(scr.leftGoalHeight + " cm OR MORE", .18 * w, .15 * h, .30 * w);

            //ctx.font = "24px Times New Roman Bold";
            //ctx.fillText("Current " + scr.leftCropType + " Crop Height", .18 * w, .24 * h, .30 * w);

            //ctx.font = "32px Times New Roman Bold";
            //ctx.fillText(scr.leftCurrentHeight + " cm", .18 * w, .27 * h, .30 * w);

            ctx.font = "24px Times New Roman";
            ctx.fillText("Months Left in Growing Season", .18 * w, .13 * h, .30 * w);

            var centerX = .18 * w;
            var centerY = .32 * h;
            var rad = .0001 * w * h;
            var clockMax = 10;
            var clockNiches = 10;

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
            ctx.fillText(scr.rightCropType, .82 * w, .04 * h, .30 * w);

            // ctx.font = "24px Times New Roman Bold";
            // ctx.fillText(scr.rightCropType + " Goal", .82 * w, .12 * h, .30 * w);

            // ctx.font = "32px Times New Roman Bold";
            // ctx.fillText(scr.rightGoalHeight + " cm OR MORE", .82 * w, .15 * h, .30 * w);

            // ctx.font = "24px Times New Roman Bold";
            // ctx.fillText("Current " + scr.rightCropType + " Crop Height", .82 * w, .24 * h, .30 * w);

            // ctx.font = "32px Times New Roman Bold";
            // ctx.fillText(scr.rightCurrentHeight + " cm", .82 * w, .27 * h, .30 * w);

            ctx.font = "24px Times New Roman";
            ctx.fillText("Months Left in Growing Season", .82 * w, .13 * h, .30 * w);

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

            var ceiling = 280; //top of scale
            var niches = 14; //number of intervals
            var topOfFrame = 0.02 * h
            var frameHeight = 0.6 * h

            var leftImageTopGap = ((ceiling - scr.leftCurrentHeight) / ceiling) * frameHeight;
            var leftImageHeight = (scr.leftCurrentHeight / ceiling) * frameHeight;
            var rightImageTopGap = ((ceiling - scr.rightCurrentHeight) / ceiling) * frameHeight;
            var rightImageHeight = (scr.rightCurrentHeight / ceiling) * frameHeight;

            //assign goal images (bullseye or skull)
            if (scr.goalType === 1){
                var topPad = 0.005*w;
                var bottomPad = 0.02*w;
                var sidePad = 0.01*w;
                if (scr.leftCurrentHeight >= scr.leftGoalHeight){
                    var leftGoalImage = plantImage[7];
                }
                if (scr.leftCurrentHeight < scr.leftGoalHeight){
                    var leftGoalImage = plantImage[6];   
                }
                if (scr.rightCurrentHeight >= scr.rightGoalHeight){
                    var rightGoalImage = plantImage[7]; 
                }
                if (scr.rightCurrentHeight < scr.rightGoalHeight){
                    var rightGoalImage = plantImage[6];   
                }
            }
            if (scr.goalType === 2){
                 var topPad = 0;
                 var bottomPad = 0.015*w;
                 var sidePad = 0;
                if (scr.leftCurrentHeight > scr.leftGoalHeight){
                    var leftGoalImage = plantImage[9];
                    //if previous height was less than goal (i.e., you just made it over the line) play the audio   
                }
                if (scr.leftCurrentHeight <= scr.leftGoalHeight){
                    var leftGoalImage = plantImage[8];   
                }
                if (scr.rightCurrentHeight >= scr.rightGoalHeight){
                    var rightGoalImage = plantImage[9];   
                    //if previous height was less than goal (i.e., you just made it over the line) play the audio
                }
                if (scr.rightCurrentHeight < scr.rightGoalHeight){
                    var rightGoalImage = plantImage[8];   
                }
            }


            if (scr.leftCropType === 'WHEAT CROP') {
                var leftImage = plantImage[0];
                //var leftImage = bluePlantImage;vvvvvv
            }
            if (scr.leftCropType === 'CORN CROP') {
                var leftImage = plantImage[1];
                //var leftImage = greenPlantImage;
            }
            if (scr.leftCropType === 'RICE CROP') {
                var leftImage = plantImage[2];
                //var leftImage = orangePlantImage;
            }
            //if (scr.leftCropType === 'BARLEY CROP') {
            //    var leftImage = plantImage[1];
                //var leftImage = redPlantImage;
            //}


            if (scr.leftCropType === 'THISTLE WEED') {
                var leftImage = plantImage[3];
                //var leftImage = bluePlantImage;
            }
            if (scr.leftCropType === 'NETTLE WEED') {
                var leftImage = plantImage[4];
                //var leftImage = greenPlantImage;
            }
            if (scr.leftCropType === 'LANTANA WEED') {
                var leftImage = plantImage[5];
                //var leftImage = orangePlantImage;
            }
            



            ctx.drawImage(leftGoalImage, .36 * w+sidePad, topOfFrame+topPad,.1 * w - 2*sidePad, topOfFrame + frameHeight*((ceiling-scr.rightGoalHeight)/ceiling) - (bottomPad+topPad)     );
            ctx.drawImage(leftImage, .36 * w, topOfFrame + leftImageTopGap, .1 * w, leftImageHeight);
            
            if (scr.rightCropType === 'WHEAT CROP') {
                var rightImage = plantImage[0];
                //var rightImage = bluePlantImage;
            }
            if (scr.rightCropType === 'CORN CROP') {
                var rightImage = plantImage[1];
                //var rightImage = greenPlantImage;
            }
            if (scr.rightCropType === 'RICE CROP') {
                var rightImage = plantImage[2];
                //var rightImage = orangePlantImage;
            }
            //if (scr.rightCropType === 'BARLEY CROP') {
             //   var rightImage = plantImage[3];
                //var rightImage = redPlantImage;
            //}

            if (scr.rightCropType === 'THISTLE WEED') {
                var rightImage = plantImage[3];
                //var leftImage = bluePlantImage;
            }
            if (scr.rightCropType === 'NETTLE WEED') {
                var rightImage = plantImage[4];
                //var leftImage = greenPlantImage;
            }
            if (scr.rightCropType === 'LANTANA WEED') {
                var rightImage = plantImage[5];
                //var leftImage = orangePlantImage;
            }
            

            ctx.drawImage(rightGoalImage, .54 * w+sidePad, topOfFrame+topPad,.1 * w - 2*sidePad, topOfFrame + frameHeight*((ceiling-scr.rightGoalHeight)/ceiling) - (bottomPad+topPad)     );
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
            var scaleTop = 0.7 * h;
            var scaleHeight = 0.22 * h;
            var scaleAbove0 = 0.5; //proportion of scale that is below 0

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
            var scaleCeiling = 160;
            var unitHeight = scaleHeight / scaleCeiling;
            var nicheInt = 20;
            var nicheHeight = unitHeight * nicheInt;
            var nicheCiel = scaleHeight / unitHeight;
            ctx.lineWidth = 2;
            var tearDropSize = 5;

            for (a = 0; a < (scr.nActions); a++) {

                

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
            ctx.fillText('Treatment ' + (a+1), scaleCenter[a], .66 * h, .30 * w);


                ctx.lineWidth = 2;
                if (a === 0){
                    ctx.fillStyle = scr.leftCropColor;
                    ctx.beginPath();
                    ctx.moveTo(scaleCenter[a]-barSpread/2 - normpdf(nicheCiel - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenLeft,scr.sdLeftGrowthGivenLeft)*scaleWidth*tearDropSize, scaleTop);
                    //ctx.fillStyle = "black";
                    for (var i = nicheCiel ; i > -1; i --) {    
                        ctx.lineTo(scaleCenter[a]-barSpread/2 - normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenLeft,scr.sdLeftGrowthGivenLeft)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);    
                    } 

                    for (var i = 0 ; i < nicheCiel+1; i++) {
                        ctx.lineTo(scaleCenter[a]-barSpread/2 + normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenLeft,scr.sdLeftGrowthGivenLeft)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);        
                    }  

                    ctx.stroke();
                    ctx.fill();

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.beginPath();
                    ctx.moveTo(scaleCenter[a]+barSpread/2 - normpdf(nicheCiel - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenLeft,scr.sdRightGrowthGivenLeft)*scaleWidth*tearDropSize, scaleTop);
                    //ctx.fillStyle = "black";
                    for (var i = nicheCiel ; i > -1; i --) {    
                        ctx.lineTo(scaleCenter[a]+barSpread/2 - normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenLeft,scr.sdRightGrowthGivenLeft)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);    
                    } 

                    for (var i = 0 ; i < nicheCiel+1; i++) {
                        ctx.lineTo(scaleCenter[a]+barSpread/2 + normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenLeft,scr.sdRightGrowthGivenLeft)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);        
                    }  

                    ctx.stroke();
                    ctx.fill();

                    //Get 1 and 99 percentile of distribution
                    //ctx.moveTo(scaleTop+scaleHeight*0, scaleCenter[a]-scaleWidth/2 +       )


                     //cdf(x, mean, variance)
                    //ctx.fillText( nicheCiel , 20,100  );
                    //ctx.fillText( 1 , 10, 100 );


                    //ctx.fillStyle = scr.leftCropColor;
                    //var growthHeight = scaleHeight * (scr.meanLeftGrowthGivenLeft / nicheCiel);
                    //ctx.fillRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    //ctx.strokeRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                    //ctx.fillStyle = scr.rightCropColor;
                    //var growthHeight = scaleHeight * (scr.meanRightGrowthGivenLeft / nicheCiel);
                    // ctx.fillRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    // ctx.strokeRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                } 

                if (scr.nActions===3 && a === 1){
                    ctx.fillStyle = scr.leftCropColor;
                    ctx.beginPath();
                    ctx.moveTo(scaleCenter[a]-barSpread/2 - normpdf(nicheCiel - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenMiddle,scr.sdLeftGrowthGivenMiddle)*scaleWidth*tearDropSize, scaleTop);
                    //ctx.fillStyle = "black";
                    for (var i = nicheCiel ; i > -1; i --) {    
                        ctx.lineTo(scaleCenter[a]-barSpread/2 - normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenMiddle,scr.sdLeftGrowthGivenMiddle)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);    
                    } 

                    for (var i = 0 ; i < nicheCiel+1; i++) {
                        ctx.lineTo(scaleCenter[a]-barSpread/2 + normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenMiddle,scr.sdLeftGrowthGivenMiddle)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);        
                    }  

                    ctx.stroke();
                    ctx.fill();

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.beginPath();
                    ctx.moveTo(scaleCenter[a]+barSpread/2 - normpdf(nicheCiel - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenMiddle,scr.sdRightGrowthGivenMiddle)*scaleWidth*tearDropSize, scaleTop);
                    //ctx.fillStyle = "black";
                    for (var i = nicheCiel ; i > -1; i --) {    
                        ctx.lineTo(scaleCenter[a]+barSpread/2 - normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenMiddle,scr.sdRightGrowthGivenMiddle)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);    
                    } 

                    for (var i = 0 ; i < nicheCiel+1; i++) {
                        ctx.lineTo(scaleCenter[a]+barSpread/2 + normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenMiddle,scr.sdRightGrowthGivenMiddle)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);        
                    }  

                    ctx.stroke();
                    ctx.fill();    



                    // ctx.fillStyle = scr.leftCropColor;
                    // var growthHeight = scaleHeight * (scr.meanLeftGrowthGivenMiddle / nicheCiel);
                    // ctx.fillRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    // ctx.strokeRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                    // ctx.fillStyle = scr.rightCropColor;
                    // var growthHeight = scaleHeight * (scr.meanRightGrowthGivenMiddle / nicheCiel);
                    // ctx.fillRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    // ctx.strokeRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                } 

                if ( (scr.nActions===3 && a === 2) || (scr.nActions===2 && a === 1)  ) {
                    
                    ctx.fillStyle = scr.leftCropColor;
                    ctx.beginPath();
                    ctx.moveTo(scaleCenter[a]-barSpread/2 - normpdf(nicheCiel - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenRight,scr.sdLeftGrowthGivenRight)*scaleWidth*tearDropSize, scaleTop);
                    //ctx.fillStyle = "black";
                    for (var i = nicheCiel ; i > -1; i --) {    
                        ctx.lineTo(scaleCenter[a]-barSpread/2 - normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenRight,scr.sdLeftGrowthGivenRight)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);    
                    } 

                    for (var i = 0 ; i < nicheCiel+1; i++) {
                        ctx.lineTo(scaleCenter[a]-barSpread/2 + normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanLeftGrowthGivenRight,scr.sdLeftGrowthGivenRight)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);        
                    }  

                    ctx.stroke();
                    ctx.fill();

                    ctx.fillStyle = scr.rightCropColor;
                    ctx.beginPath();
                    ctx.moveTo(scaleCenter[a]+barSpread/2 - normpdf(nicheCiel - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenRight,scr.sdRightGrowthGivenRight)*scaleWidth*tearDropSize, scaleTop);
                    //ctx.fillStyle = "black";
                    for (var i = nicheCiel ; i > -1; i --) {    
                        ctx.lineTo(scaleCenter[a]+barSpread/2 - normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenRight,scr.sdRightGrowthGivenRight)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);    
                    } 

                    for (var i = 0 ; i < nicheCiel+1; i++) {
                        ctx.lineTo(scaleCenter[a]+barSpread/2 + normpdf(i - nicheCiel*(1-scaleAbove0),scr.meanRightGrowthGivenRight,scr.sdRightGrowthGivenRight)*scaleWidth*tearDropSize, scaleTop + scaleHeight - scaleHeight * i / nicheCiel);        
                    }  

                    ctx.stroke();
                    ctx.fill();


                    // ctx.fillStyle = scr.leftCropColor;
                    // var growthHeight = scaleHeight * (scr.meanLeftGrowthGivenRight / nicheCiel);
                    // ctx.fillRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    // ctx.strokeRect(scaleCenter[a]-barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                    // ctx.fillStyle = scr.rightCropColor;
                    // var growthHeight = scaleHeight * (scr.meanRightGrowthGivenRight / nicheCiel);
                    // ctx.fillRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);
                    // ctx.strokeRect(scaleCenter[a]+barSpread/2-barWidth/2, scaleTop + scaleHeight*scaleAbove0 - growthHeight, barWidth, growthHeight);

                } 

                //Left Axes
                ctx.moveTo(scaleCenter[a]-scaleWidth/2, scaleTop + scaleHeight*scaleAbove0);
                ctx.lineTo(scaleCenter[a]+scaleWidth/2, scaleTop + scaleHeight*scaleAbove0);
                ctx.stroke();

                ctx.moveTo(scaleCenter[a]-scaleWidth/2+0.01*w , scaleTop - .02 * h); //extra 0.02*h bit gives some extra length above and below topmost and bottomost niches
                ctx.lineTo(scaleCenter[a]-scaleWidth/2+0.01*w , scaleTop + scaleHeight + .02 * h);
                ctx.stroke();

                //draw crop labels under bars
                ctx.font = "18px Times New Roman";
                ctx.fillStyle = scr.leftCropColor;
                ctx.fillText(scr.leftCropTypeShort, scaleCenter[a]-barSpread/2, scaleTop + scaleHeight + 0.015*h , .30 * w);
                ctx.fillStyle = scr.rightCropColor;
                ctx.fillText(scr.rightCropTypeShort, scaleCenter[a]+barSpread/2, scaleTop + scaleHeight + 0.015*h , .30 * w);




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

        function drawExclamationMark(canvas, ctx, scr) {

            var w = canvas.width;
            var h = canvas.height;

            ctx.font = "48px Times New Roman";
            ctx.fillStyle = scr.leftCropColor;
            
            if(scr.goalType === 1){
                if (scr.leftCurrentHeight >= scr.leftGoalHeight) {
                    ctx.fillText(String.fromCharCode(10071), .34 * w, .08 * h);
                }
                if (scr.rightCurrentHeight >= scr.rightGoalHeight) {
                    ctx.fillText(String.fromCharCode(10071), .66 * w, .08 * h);
                }
            }

            if(scr.goalType === 2){
                if (scr.leftCurrentHeight > scr.leftGoalHeight) {
                    ctx.fillText(String.fromCharCode(10071), .34 * w, .08 * h);
                }
                if (scr.rightCurrentHeight > scr.rightGoalHeight) {
                    ctx.fillText(String.fromCharCode(10071), .66 * w, .08 * h);
                }
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

        function cdf(x, mean, variance) {
            return 0.5 * (1 + erf((x - mean) / (Math.sqrt(2 * variance))));
        }

        function erf(x) {
            // save the sign of x
            var sign = (x >= 0) ? 1 : -1;
            x = Math.abs(x);

            // constants
            var a1 =  0.254829592;
            var a2 = -0.284496736;
            var a3 =  1.421413741;
            var a4 = -1.453152027;
            var a5 =  1.061405429;
            var p  =  0.3275911;

            // A&S formula 7.1.26
            var t = 1.0/(1.0 + p*x);
            var y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-x * x);
            return sign * y; // erf(-x) = -erf(x);
        }

        function normpdf(x, mean, std) {
            return (1/(std * Math.sqrt(2 * Math.PI))) * Math.exp(  -1*( Math.pow(x-mean,2)/Math.pow(2 * std,2) )   );
        }    

        //Run Trial
        display_element.html('<canvas height="900" id="canvas" width="1200">Your browser doesn&#39;t support canvas</canvas>');

        //load audio
        var audioApproach = new Audio("js/sounds/chaching.mp3");
        var audioAvoidance = new Audio("js/sounds/failbuzzer.mp3");

        //load images
        var plantImage = new Array();
        plantImage[0] = new Image();
        plantImage[0].src = "js/img/darkgreenPlant.png";
        plantImage[1] = new Image();
        plantImage[1].src = "js/img/yellowgreenPlant.png";
        plantImage[2] = new Image();
        plantImage[2].src = "js/img/lightseagreenPlant.png";
        plantImage[3] = new Image();
        plantImage[3].src = "js/img/saddlebrownPlant.png";
        plantImage[4] = new Image();
        plantImage[4].src = "js/img/goldenrodPlant.png";
        plantImage[5] = new Image();
        plantImage[5].src = "js/img/coralPlant.png";
        plantImage[6] = new Image();
        plantImage[6].src = "js/img/treasure-chest.png";
        plantImage[7] = new Image();
        plantImage[7].src = "js/img/treasureChestGold.png";
        plantImage[8] = new Image();
        plantImage[8].src = "js/img/skull.png";
        plantImage[9] = new Image();
        plantImage[9].src = "js/img/skull copy_red.png";


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
                ctx.clearRect(.01 * w, .64 * h, .98 * w, .35 * h);
                //ctx.clearRect(.3 * w, .73 * h, .4 * w, .26 * h);

                ctx.fillStyle = "blue";
                ctx.lineWidth = 3;
                ctx.font = "36px Times New Roman";
                ctx.fillText("On this trial, you must manage the", .18 * w, .4 * h, .32 * w);
                ctx.fillText(scr.leftCropType + " and " + scr.rightCropType + ".", .18 * w, .44 * h, .32 * w);

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