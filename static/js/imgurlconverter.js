"use strict"
define(["rpg/store"], function(store) {
	return {
		_prefix : "static/image/",
		_splitting : function (url) {
			return this._prefix + url.substr(0,2) + "/" + url.substr(2,2) + "/";
		},
		_base : function (url) {
			return this._splitting(url) + url;
		},
		small : function (url) {
			return this._base(url) + ".small";
		},
		medium : function (url) {
			return this._base(url) + ".medium";
		},
		orig : function (url, id) {
			var orig = store.image.get(id).orig;

			return this._splitting(url) + orig + ":" + url + ".orig";
		}
	};
});

