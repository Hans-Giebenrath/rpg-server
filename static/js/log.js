"use strict"
define(["dojo/dom",
	    "rpg/modal"
], function(dom, modal) {
	return {
		error : function(msg, o) {
			msg = msg || o.response.data;
			console.error(msg, o);
			var m = modal("An Error occured.", { "desc" : msg}, [], [ { "Ok, I got it." : function(inputs, close) { close(); }}, {"No, I didn't got it." : function() { alert("Then read again."); }}], null, function () { m.destroy(); });
			m.show();
			// TODO do some user-stuff
		},
		info : function(msg) {
			var m = modal("An information for you came in.", { "desc" : msg}, [], [ { "Ok, I got it." : function(inputs, close) { close(); }}, {"No, I didn't got it." : function() { alert("Then read again."); }}], null, function () { m.destroy(); });
			// TODO do some user-stuff
		},
		success : function(msg) {
			// TODO do some user-stuff
		}
	};
});
