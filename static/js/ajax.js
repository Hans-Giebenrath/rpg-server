"use strict"
define(["dojo/request",
		 "rpg/log"
], function (request, log) {
	//var self = {
		//send : function (method, data, success, error) {
	var send = function (method, data, success, error) {
		var q = {};
		var i = 1;
		data.forEach(function(item) {
			q["arg" + i] = item;
			i = i + 1;
		});
		request("/ajax/" + method, {
			data : q,
			handleAs : "json",
			preventCache : true,
			method : "POST"
		}).then(function (data) {
			if (typeof success === "function") {
				if (data == "null") {
					data = null;
				}
			   success(data);
			}
		}, function (err) {
			log.error(err.response.message, err);
			if (typeof error === "function") error();
		});
	}
	return send;
});
