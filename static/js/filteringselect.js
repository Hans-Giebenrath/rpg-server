define(["dojo/dom",
	    "dojo/dom-construct",
		"rpg/list"
], function(dom, construct, list) {
	var create_option = function(attr_value, attr_display, item) {
		return construct.toDom("<option value=\"" + item[attr_value] + "\">" + item[attr_display] + "</option>");
	};

	return function (id, observable, attr_value, attr_display) {
		var select = construct.place("<select></select>", id, "first");

		var destr = list(select, observable, function(item) { return create_option(attr_value, attr_display, item); });

		return {
			value : function() {
				return select.value;
			},
			destroy : function () {
				destr();
				construct.empty(id);
			}
		};
	};
});
