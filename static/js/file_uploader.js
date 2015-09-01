"use strict"
define(["dojo/request"
], function(request) {
	return function (input, onsuccess, onerror) {
		var fd = new FormData();
		fd.append("filename", input.files[0], "image");
		request("/upload/", {
			data : fd,
			handleAs : "json",
			method : "POST"
		}).then(function (data) {
			if (typeof onsuccess === "function") onsuccess(data);
		}, function (err) {
			log.error(err.response.message, err);
			if (typeof onerror === "function") onerror();
		});

	};
});
