$(document).ready(function() {
// OPENCPU JS
//calls R function: for risk calculation
$("#postbutton").click(function() {
        
  var req1 = ocpu.call("config_read", {}, 
  					function(session1) {
		//read the session properties (just for fun)
        $("#key_config").text(session1.getKey());
        $("#location_config").text(session1.getLoc());
    				var req2 = ocpu.call("simulate_risk", 
            											{config: session1}, 
                                  function(session2) {
       
        //read the session properties (just for fun)
        $("#key_risk").text(session2.getKey());
        $("#location_risk").text(session2.getLoc());
      														//retrieve session console (async)
      														session2.getConsole(function(outtxt) {
        														$("#output").text(outtxt);});
      														$("#plotdiv").rplot("plot_inflow",
        																				 			{risk: session2})
    															});
  											});
	});
});