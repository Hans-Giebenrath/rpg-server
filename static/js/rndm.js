"use strict"
define([], function() {
	return function () {
		return Math.random().toString(36).substr(2);
	};
});
