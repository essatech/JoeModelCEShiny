
function leaflet_hover_functions(ns_prefix) {

		var mapobj = document.getElementById(ns_prefix + "mymap");
		// mapobj = document.getElementById("main_map-mymap");
		console.log("ns_prefix...");
		console.log("#" + ns_prefix + "mymap");
		
		if(mapobj) {
			
			console.log("trigger leaflet_hover_functions()");
			
			mapobj.addEventListener('mousemove', function(event) {
			  //console.log(event, geoLayer, mapobj);
			  console.log(mapobj);
			});

		} else {
			
			console.log("Map not yet loaded...");
		}

}

