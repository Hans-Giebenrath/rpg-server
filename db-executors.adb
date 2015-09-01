with Interfaces.C; use Interfaces.C;
with db.connections; use db.connections;
with db.stmts; use db.stmts;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with util; use util;

package body Db.Executors is
	procedure Execute (E : in out Void) is
	begin
		log ("[db.executors.void.execute] ", Statement_Name(E.Stmt_To_Execute), debug);

		E.Connection := Reserve_Connection; -- lazy connection.
		E.Result := Execute_Prepared(E.Stmt_to_execute, E.Connection, E.Param_Values.all);
	exception
		when Error : others =>
			log_exception(Error, "[db.executors.execute void]");
			raise;
	end;

	procedure Execute (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is
		E : Void (Param_Values, Stmt_To_Execute);
	begin
		Execute (E);
	end Execute;

	procedure Fill (O: in out Single) is
		RD : Row_Data_Access renames O.Row;
	begin
		for I in RD'Range loop
			RD(I) := Row_To_Unbounded(
				O.Result,
				O.Active_Tuple,
				I - RD'First
			);
		end loop;
	end Fill;

	procedure  Execute (E : in out Single) is
		function PQntuples (Res : PGresult) return int;
		pragma Import (C, PQntuples, "PQntuples");
		function PQnfields (Res : PGresult) return int;
		pragma Import (C, PQnfields, "PQnfields");
	begin
		log ("[db.executors.record.execute]", Statement_Name(E.Stmt_To_Execute), debug);

		Execute(Void(E));

		E.Total_Tuples := Natural(PQntuples(E.Result));

		log ("[db.iterators.first] ", Statement_Name(E.Stmt_To_Execute) & "," & Natural'Image(E.Total_Tuples) & " elements found.", debug);
		
		if E.Total_Tuples = 0 then
			raise No_Lines_Found with "No lines were found.";
		end if;

		E.Row := new Row_Data(1 .. Natural(PQnfields(E.Result)));

		Fill(E);
	exception
		when Error : others =>
			log_exception(Error, "[db.executors.execute single]");
			raise;
	end Execute;

	function Execute (E: in out Single) return Row_Data_Access is
	begin
		Execute(E);
		return E.Row;
	end Execute;

	function Execute (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) return Single is
	begin
		return Result : Single (Param_Values, Stmt_To_Execute) do
			-- TODO look for pragma warning unused variable,
			-- or for extension aggregate with discriminant
			Result.Active_Tuple := 0;
		end return;
	end Execute;

	overriding
	procedure Finalize (E : in out Void) is
		procedure PQclear (Res : PGresult);
		pragma Import (C, PQclear, "PQclear");
	begin
		log ("[db.e.finalize.void]", "", debug);

		if E.Connection /= Null_Connection then
			Release_Connection(E.Connection);
		end if;

		PQclear(E.Result);
	end Finalize;

	overriding
	procedure Finalize (E : in out Single) is
		procedure Free is new Ada.Unchecked_Deallocation(Row_Data, Row_Data_Access);
	begin
		log ("[db.e.finalize.single]", "", debug);
		Free (E.Row);

		Finalize(Void(E));
	end Finalize;

	function Row_Value (E : in out Single'Class; Idx : Natural) return Row_Data_Reference is
	begin
		Return Result : Row_Data_Reference (E.Row(Idx)'Access) do
			null;
		end return;
	end Row_Value;

	function Has_Element (Pos : Cursor) return Boolean is
		(Pos /= No_Element);

	function Row_Value (Container : in out Table; Pos : Cursor)
		return Row_Data_Access is
		(Pos.Element);

	overriding function First (Object : Iterator) return Cursor is
		E : Table renames Object.Container.all;
	begin
		log ("[db.executors.first] ", Statement_Name(E.Stmt_To_Execute), debug);

		Execute (Single(E));

		return Cursor'(Element => E.Row);
	exception
		when Error : others =>
			log_exception(Error, "[db.executors.first]");
			raise;
	end;

	overriding function Next (Object : Iterator; Position : Cursor) return Cursor is
		E : Table renames Object.Container.all;
	begin
		log ("[db.executors.next] ", Statement_Name(E.Stmt_To_Execute), debug);

		E.Active_Tuple := E.Active_Tuple + 1;

		if E.Active_Tuple >= E.Total_Tuples then
			return No_Element;
		end if;

		Fill (Single(E));

		return Cursor'(Element => E.Row);
	exception
		when Error : others =>
			log_exception(Error, "[db.executors.next]");
			raise;
	end Next;

	function Iterate (Container : Table)
		return List_Iterator_Interfaces.Forward_Iterator'Class is
	begin
		return Result : Iterator do
			Result.Container := Container'Unrestricted_Access;
		end return;
	end Iterate;
end Db.Executors;
