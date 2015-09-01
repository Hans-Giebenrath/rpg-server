with db; use db;
with db.connections; use db.connections;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with util; use util;

package body db.stmts is
	function Statement_Name (Stmt : Statement) return String is
		(Statement_Prefix & Statement'Image(Stmt));

	procedure Init (Connection_String_P : String; Number_Of_Connections : Natural) is
		procedure Prepare_Stmt_For_Connection (Connection : PGconnection) is
		begin
			for s in Statement'Range loop
				prepare_stmt (s, Connection);
			end loop;
		end Prepare_Stmt_For_Connection;
	begin
		Init_Connections(Connection_String_P, Number_Of_Connections);
		Query_Connections(Prepare_Stmt_For_Connection'Access);
	end Init;

	procedure prepare_stmt (stmt_to_prepare : Statement; connection : PGconnection) is
		function PQprepare (
			Conn    : PGconnection;
			Name    : chars_ptr;
			Query   : chars_ptr;
			nParams : Natural := 0;
			param_types : access chars_ptr
		) return PGresult;
		pragma Import (C, PQprepare, "PQprepare");

		Result : PGresult;

		function Create_Parameterlist (Number : Natural) return String is
			Buffer : Unbounded_String;
		begin
			for I in 1 .. Number loop
				declare
					N_Tmp : String := Natural'Image(I);
					N : String := N_Tmp(N_Tmp'First + 1 .. N_Tmp'Last);
				begin
					if I = 1 then
						Append(Buffer, "$" & N);
					else
						Append(Buffer, ", $" & N);
					end if;
				end;
			end loop;
			return " (" & To_String(Buffer) & ")";
		end Create_Parameterlist;

		Query : String := "Select * From " & Statement'Image(Stmt_To_Prepare) & Create_Parameterlist(Statement_Parameters(Stmt_To_Prepare).Parameters);
	begin
		Result :=
			PQprepare (
				Connection,
				New_String(Statement_Name(Stmt_to_Prepare)),
				-- +3 : that is because userid, groupid and characterid are always passed, too.
				New_String(Query),
				Statement_Parameters(Stmt_To_Prepare).Parameters,
				null
			);

		-- TODO include it in production
		-- log ("[db.stmts.prepare_stmt] Statement prepared. Name: ", Statement'Image(Stmt_to_Prepare) & ", Query: " & Query, debug);

		Check_For_Errors (Result);
	end prepare_stmt;

	function Execute_Prepared_Scalar (stmt_to_execute : Statement; connection : PGconnection; param_values : param_values_t) return Unbounded_String is
	begin
		Return Row_To_Unbounded(execute_prepared(stmt_to_execute, connection, param_values), 0, 0);
	end Execute_Prepared_Scalar;

	procedure Execute_Prepared_Void(Stmt_To_Execute : Statement; connection : PGconnection; param_values : param_values_t) is
		Result : PGResult;
	begin
		Result := Execute_Prepared(stmt_to_execute, connection, param_values);
	end Execute_Prepared_Void;

	function execute_prepared (Stmt_To_Execute : Statement; connection : PGconnection; param_values : param_values_t) return DB.PGresult is
		Result : DB.PGresult;
		function PQexecPrepared (
			Conn    : PGconnection;
			Name    : chars_ptr;
			Nparams : Natural := 0;
			Values  : param_values_t;
			-- Values  : access chars_ptr := null;
			Lengths : access int := null;
			Formats : access int := null;
			Format  : Natural := 0
		) return PGresult;
		pragma Import (C, PQexecPrepared, "PQexecPrepared");
	begin
		if Param_values'Length = 0 then
			Result := PQexecPrepared (
				Conn => Connection,
				Name => New_String(Statement_Name(Stmt_To_Execute)),
				Values => param_values
			);
		else
            Result := PQexecPrepared (
				Conn => Connection,
				Name => New_String(Statement_Name(Stmt_to_Execute)),
				Nparams => Statement_Parameters(Stmt_To_Execute).Parameters,
				Values => Param_Values
				-- Values => values'access
			);
		end if;

		Check_For_Errors(Result);

		log ("[db.stmts.execute_prepared] Executed ", Statement_Name(Stmt_To_Execute) & " with" & Natural'Image(Statement_Parameters(Stmt_To_Execute).Parameters) & " parameters.", debug);

		return Result;
	end execute_prepared;

end db.stmts;
