"use strict"
define({
	none : { value : 0, label : "None" },
	read : { value : 100, label : "Read" },
	edit : { value : 200, label : "Modify" },
	// equal to edit. Will be skipped during user_rights enumeration
	add : { value : 200, label : "Add" },
	"delete" : { value : 300, label : "Delete" },
	master : { value : 400, label : "Master"}
});
