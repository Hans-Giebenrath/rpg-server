with ada.containers.vectors; use ada.containers;

package db.connections is
	function Reserve_Connection return PGconnection;
	procedure Release_Connection (Connection : PGconnection);

	function initialized return boolean;
	procedure Init_Connections (Connection_String_P : String; Number_Of_Connections : Natural)
		with
			Pre => not initialized,
			Post => initialized;
		

	-- TODO
	-- procedure Increment_Number_of_Parallel_Connections;
	-- procedure Decrement_Number_of_Parallel_Connections;
	procedure Set_Number_Of_Parallel_Connections (Number_Of_Parallel_Connections : Natural);
	function Get_Number_Of_Parallel_Connections return Natural;

	procedure Check_Connection_Status (Connection : PGconnection);

	procedure Query_Connections (Proc : not null access procedure (Connection : PGconnection));

	-- TODO Create callbacks to register, if a connection will be established
	-- why? ...
private
	package connection_vectors is new vectors(
		Index_Type => Natural,
		Element_Type => PGconnection
	);
	use connection_vectors;

	connections : connection_vectors.vector;
end db.connections;
