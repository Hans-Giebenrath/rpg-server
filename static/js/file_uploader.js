/*
	Copyright (C) 2015 Hans Giebenrath

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

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
