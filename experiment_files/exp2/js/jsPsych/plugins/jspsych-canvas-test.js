/**
 * jsPsych plugin for showing animations that mimic the experiment described in
 * 
 * Fiser, J., & Aslin, R. N. (2002). Statistical learning of higher-order 
 * temporal structure from visual shape sequences. Journal of Experimental 
 * Psychology: Learning, Memory, and Cognition, 28(3), 458.
 * 
 * Josh de Leeuw
 * 
 * documentation: https://github.com/jodeleeuw/jsPsych/wiki/jspsych-vsl-animate-occlusion
 * 
 */

jsPsych['canvas-test'] = (function(){

  var plugin = {};

  plugin.create = function(params){
    var trials = [];

    trials.push({});

    return trials;
  }

  plugin.trial = function(display_element, trial){
        
        var canvas = document.getElementById("canvas");
        var ctx = canvas.getContext("2d");

        ctx.fillText("This is my canvas.", 100, 100, 100);

        var after_response = function(info){
            alert('You pressed key '+info.key+' after '+info.rt+'ms');
            jsPsych.finishTrial();
        }

        jsPsych.pluginAPI.getKeyboardResponse(after_response, [], 'date', false);
  }

  return plugin;

})();
