"use strict"
require(["dojo/dom",
	   "dojo/router",
	   "dojo/topic",
	   "rpg/soul",
	   "rpg/tabpane",
	   "rpg/store",
	   "rpg/gallery_pane",
	   "rpg/permission_mapping",
	   "dojo/domReady!"
], function(dom, router, topic, soul, tabpane, store, gallery_pane, permission_mapping) {
	var client_gallery;
	var group_gallery;
	client_gallery = gallery_pane("char-gallery", "character");
	group_gallery = gallery_pane("group-gallery", "group");
	topic.subscribe("enter_group", function() {
		// the group_gallery will barely change
		group_gallery(store.gallery.query({gallery_broker : soul.group_gallery}), "", soul.group_id, permission_mapping.master.value);
	});

	router.register(/\/char\/(\w*)/, function(arg) {
		client_gallery(store.gallery.query({gallery_broker : store.group_chars.get(arg.params[0]).gallery}), "/char/" + arg.params[0], arg.params[0], soul.character_id == parseInt(arg.params[0]) ? permission_mapping.master.value : 0);
	});
	router.register(/\/gallery(?:\/(\w+)(?:\/(\w+))?)?/, function(arg) {
	//router.register(/^\/gallery/, function() {
		tabpane.change("gallery"); // activates the char tab-group
	});
	/* router.register(/^\/gallery\/(\w*)/, function(arg) {
		load(arg.params.id, null);
	});*/
});
