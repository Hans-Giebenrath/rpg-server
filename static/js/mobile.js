require(["dojo/dom",
		 "dojo/dom-class",
		 "dojo/on",
		 "dojo/hash",
		 "dojo/topic",
		 "dojo/domReady!"
], function(dom, dom_class, on, hash, topic) {
	nodes = {
		cb1 : dom.byId("all-my-roles-cb"),
		cb2 : dom.byId("this-group-cb"),
		cb3 : dom.byId("misc-cb")
	};

	topic.subscribe("/dojo/hashchange", function(changedHash){
		nodes.cb1.checked = false;
		nodes.cb2.checked = false;
		nodes.cb3.checked = false;
	});

	nodes.cb1.onchange = function() {
		nodes.cb2.checked = false;
		nodes.cb3.checked = false;
	};
	nodes.cb2.onchange = function() {
		nodes.cb1.checked = false;
		nodes.cb3.checked = false;
	};
	nodes.cb3.onchange = function() {
		nodes.cb1.checked = false;
		nodes.cb2.checked = false;
	};
});
