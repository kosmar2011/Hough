
--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_in_wait_v1.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
--------------------------------------------------------------------------------

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

PACKAGE ccs_in_wait_pkg_v1 IS

COMPONENT ccs_in_wait_v1
  GENERIC (
    rscid    : INTEGER;
    width    : INTEGER
  );
  PORT (
    idat   : OUT std_logic_vector(width-1 DOWNTO 0);
    rdy    : OUT std_logic;
    ivld   : OUT std_logic;
    dat    : IN  std_logic_vector(width-1 DOWNTO 0);
    irdy   : IN  std_logic;
    vld    : IN  std_logic
   );
END COMPONENT;

END ccs_in_wait_pkg_v1;

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all; -- Prevent STARC 2.1.1.2 violation

ENTITY ccs_in_wait_v1 IS
  GENERIC (
    rscid : INTEGER;
    width : INTEGER
  );
  PORT (
    idat  : OUT std_logic_vector(width-1 DOWNTO 0);
    rdy   : OUT std_logic;
    ivld  : OUT std_logic;
    dat   : IN  std_logic_vector(width-1 DOWNTO 0);
    irdy  : IN  std_logic;
    vld   : IN  std_logic
  );
END ccs_in_wait_v1;

ARCHITECTURE beh OF ccs_in_wait_v1 IS
BEGIN

  idat <= dat;
  rdy  <= irdy;
  ivld <= vld;

END beh;


--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_out_wait_v1.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
--------------------------------------------------------------------------------

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

PACKAGE ccs_out_wait_pkg_v1 IS

COMPONENT ccs_out_wait_v1
  GENERIC (
    rscid    : INTEGER;
    width    : INTEGER
  );
  PORT (
    dat    : OUT std_logic_vector(width-1 DOWNTO 0);
    irdy   : OUT std_logic;
    vld    : OUT std_logic;
    idat   : IN  std_logic_vector(width-1 DOWNTO 0);
    rdy    : IN  std_logic;
    ivld   : IN  std_logic
  );
END COMPONENT;

END ccs_out_wait_pkg_v1;

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all; -- Prevent STARC 2.1.1.2 violation

ENTITY ccs_out_wait_v1 IS
  GENERIC (
    rscid : INTEGER;
    width : INTEGER
  );
  PORT (
    dat   : OUT std_logic_vector(width-1 DOWNTO 0);
    irdy  : OUT std_logic;
    vld   : OUT std_logic;
    idat  : IN  std_logic_vector(width-1 DOWNTO 0);
    rdy   : IN  std_logic;
    ivld  : IN  std_logic
  );
END ccs_out_wait_v1;

ARCHITECTURE beh OF ccs_out_wait_v1 IS
BEGIN

  dat  <= idat;
  irdy <= rdy;
  vld  <= ivld;

END beh;


--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/mgc_io_sync_v2.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
--------------------------------------------------------------------------------

LIBRARY ieee;

USE ieee.std_logic_1164.all;
PACKAGE mgc_io_sync_pkg_v2 IS

COMPONENT mgc_io_sync_v2
  GENERIC (
    valid    : INTEGER RANGE 0 TO 1
  );
  PORT (
    ld       : IN    std_logic;
    lz       : OUT   std_logic
  );
END COMPONENT;

END mgc_io_sync_pkg_v2;

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all; -- Prevent STARC 2.1.1.2 violation

ENTITY mgc_io_sync_v2 IS
  GENERIC (
    valid    : INTEGER RANGE 0 TO 1
  );
  PORT (
    ld       : IN    std_logic;
    lz       : OUT   std_logic
  );
END mgc_io_sync_v2;

ARCHITECTURE beh OF mgc_io_sync_v2 IS
BEGIN

  lz <= ld;

END beh;


--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/hls_pkgs/src/funcs.vhd 

-- a package of attributes that give verification tools a hint about
-- the function being implemented
PACKAGE attributes IS
  ATTRIBUTE CALYPTO_FUNC : string;
  ATTRIBUTE CALYPTO_DATA_ORDER : string;
end attributes;

-----------------------------------------------------------------------
-- Package that declares synthesizable functions needed for RTL output
-----------------------------------------------------------------------

LIBRARY ieee;

use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

PACKAGE funcs IS

-----------------------------------------------------------------
-- utility functions
-----------------------------------------------------------------

   FUNCTION TO_STDLOGIC(arg1: BOOLEAN) RETURN STD_LOGIC;
--   FUNCTION TO_STDLOGIC(arg1: STD_ULOGIC_VECTOR(0 DOWNTO 0)) RETURN STD_LOGIC;
   FUNCTION TO_STDLOGIC(arg1: STD_LOGIC_VECTOR(0 DOWNTO 0)) RETURN STD_LOGIC;
   FUNCTION TO_STDLOGIC(arg1: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION TO_STDLOGIC(arg1: SIGNED(0 DOWNTO 0)) RETURN STD_LOGIC;
   FUNCTION TO_STDLOGICVECTOR(arg1: STD_LOGIC) RETURN STD_LOGIC_VECTOR;

   FUNCTION maximum(arg1, arg2 : INTEGER) RETURN INTEGER;
   FUNCTION minimum(arg1, arg2 : INTEGER) RETURN INTEGER;
   FUNCTION ifeqsel(arg1, arg2, seleq, selne : INTEGER) RETURN INTEGER;
   FUNCTION resolve_std_logic_vector(input1: STD_LOGIC_VECTOR; input2 : STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR;
   
-----------------------------------------------------------------
-- logic functions
-----------------------------------------------------------------

   FUNCTION and_s(inputs: STD_LOGIC_VECTOR) RETURN STD_LOGIC;
   FUNCTION or_s (inputs: STD_LOGIC_VECTOR) RETURN STD_LOGIC;
   FUNCTION xor_s(inputs: STD_LOGIC_VECTOR) RETURN STD_LOGIC;

   FUNCTION and_v(inputs: STD_LOGIC_VECTOR; olen: POSITIVE) RETURN STD_LOGIC_VECTOR;
   FUNCTION or_v (inputs: STD_LOGIC_VECTOR; olen: POSITIVE) RETURN STD_LOGIC_VECTOR;
   FUNCTION xor_v(inputs: STD_LOGIC_VECTOR; olen: POSITIVE) RETURN STD_LOGIC_VECTOR;

-----------------------------------------------------------------
-- mux functions
-----------------------------------------------------------------

   FUNCTION mux_s(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC       ) RETURN STD_LOGIC;
   FUNCTION mux_s(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR) RETURN STD_LOGIC;
   FUNCTION mux_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC       ) RETURN STD_LOGIC_VECTOR;
   FUNCTION mux_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR;

-----------------------------------------------------------------
-- latch functions
-----------------------------------------------------------------
   FUNCTION lat_s(dinput: STD_LOGIC       ; clk: STD_LOGIC; doutput: STD_LOGIC       ) RETURN STD_LOGIC;
   FUNCTION lat_v(dinput: STD_LOGIC_VECTOR; clk: STD_LOGIC; doutput: STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR;

-----------------------------------------------------------------
-- tristate functions
-----------------------------------------------------------------
--   FUNCTION tri_s(dinput: STD_LOGIC       ; control: STD_LOGIC) RETURN STD_LOGIC;
--   FUNCTION tri_v(dinput: STD_LOGIC_VECTOR; control: STD_LOGIC) RETURN STD_LOGIC_VECTOR;

-----------------------------------------------------------------
-- compare functions returning STD_LOGIC
-- in contrast to the functions returning boolean
-----------------------------------------------------------------

   FUNCTION "=" (l, r: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION "=" (l, r: SIGNED  ) RETURN STD_LOGIC;
   FUNCTION "/="(l, r: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION "/="(l, r: SIGNED  ) RETURN STD_LOGIC;
   FUNCTION "<="(l, r: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION "<="(l, r: SIGNED  ) RETURN STD_LOGIC;
   FUNCTION "<" (l, r: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION "<" (l, r: SIGNED  ) RETURN STD_LOGIC;
   FUNCTION ">="(l, r: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION ">="(l, r: SIGNED  ) RETURN STD_LOGIC;
   FUNCTION ">" (l, r: UNSIGNED) RETURN STD_LOGIC;
   FUNCTION ">" (l, r: SIGNED  ) RETURN STD_LOGIC;

   -- RETURN 2 bits (left => lt, right => eq)
   FUNCTION cmp (l, r: STD_LOGIC_VECTOR) RETURN STD_LOGIC;

-----------------------------------------------------------------
-- Vectorized Overloaded Arithmetic Operators
-----------------------------------------------------------------

   FUNCTION faccu(arg: UNSIGNED; width: NATURAL) RETURN UNSIGNED;
 
   FUNCTION fabs(arg1: SIGNED  ) RETURN UNSIGNED;

   FUNCTION "/"  (l, r: UNSIGNED) RETURN UNSIGNED;
   FUNCTION "MOD"(l, r: UNSIGNED) RETURN UNSIGNED;
   FUNCTION "REM"(l, r: UNSIGNED) RETURN UNSIGNED;
   FUNCTION "**" (l, r: UNSIGNED) RETURN UNSIGNED;

   FUNCTION "/"  (l, r: SIGNED  ) RETURN SIGNED  ;
   FUNCTION "MOD"(l, r: SIGNED  ) RETURN SIGNED  ;
   FUNCTION "REM"(l, r: SIGNED  ) RETURN SIGNED  ;
   FUNCTION "**" (l, r: SIGNED  ) RETURN SIGNED  ;

-----------------------------------------------------------------
--               S H I F T   F U C T I O N S
-- negative shift shifts the opposite direction
-- *_stdar functions use shift functions from std_logic_arith
-----------------------------------------------------------------

   FUNCTION fshl(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshl(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED;

   FUNCTION fshl(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED  ;
   FUNCTION fshr(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED  ;
   FUNCTION fshl(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED  ;
   FUNCTION fshr(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED  ;

   FUNCTION fshl(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshl(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;

   FUNCTION frot(arg1: STD_LOGIC_VECTOR; arg2: STD_LOGIC_VECTOR; signd2: BOOLEAN; sdir: INTEGER range -1 TO 1) RETURN STD_LOGIC_VECTOR;
   FUNCTION frol(arg1: STD_LOGIC_VECTOR; arg2: UNSIGNED) RETURN STD_LOGIC_VECTOR;
   FUNCTION fror(arg1: STD_LOGIC_VECTOR; arg2: UNSIGNED) RETURN STD_LOGIC_VECTOR;
   FUNCTION frol(arg1: STD_LOGIC_VECTOR; arg2: SIGNED  ) RETURN STD_LOGIC_VECTOR;
   FUNCTION fror(arg1: STD_LOGIC_VECTOR; arg2: SIGNED  ) RETURN STD_LOGIC_VECTOR;

   -----------------------------------------------------------------
   -- *_stdar functions use shift functions from std_logic_arith
   -----------------------------------------------------------------
   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED;

   FUNCTION fshl_stdar(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED  ;
   FUNCTION fshr_stdar(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED  ;
   FUNCTION fshl_stdar(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED  ;
   FUNCTION fshr_stdar(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED  ;

   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;
   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED;

-----------------------------------------------------------------
-- indexing functions: LSB always has index 0
-----------------------------------------------------------------

   FUNCTION readindex(vec: STD_LOGIC_VECTOR; index: INTEGER                 ) RETURN STD_LOGIC;
   FUNCTION readslice(vec: STD_LOGIC_VECTOR; index: INTEGER; width: POSITIVE) RETURN STD_LOGIC_VECTOR;

   FUNCTION writeindex(vec: STD_LOGIC_VECTOR; dinput: STD_LOGIC       ; index: INTEGER) RETURN STD_LOGIC_VECTOR;
   FUNCTION n_bits(p: NATURAL) RETURN POSITIVE;
   --FUNCTION writeslice(vec: STD_LOGIC_VECTOR; dinput: STD_LOGIC_VECTOR; index: INTEGER) RETURN STD_LOGIC_VECTOR;
   FUNCTION writeslice(vec: STD_LOGIC_VECTOR; dinput: STD_LOGIC_VECTOR; enable: STD_LOGIC_VECTOR; byte_width: INTEGER;  index: INTEGER) RETURN STD_LOGIC_VECTOR ;

   FUNCTION ceil_log2(size : NATURAL) return NATURAL;
   FUNCTION bits(size : NATURAL) return NATURAL;    

   PROCEDURE csa(a, b, c: IN INTEGER; s, cout: OUT STD_LOGIC_VECTOR);
   PROCEDURE csha(a, b: IN INTEGER; s, cout: OUT STD_LOGIC_VECTOR);
   
END funcs;


--------------------------- B O D Y ----------------------------


PACKAGE BODY funcs IS

-----------------------------------------------------------------
-- utility functions
-----------------------------------------------------------------

   FUNCTION TO_STDLOGIC(arg1: BOOLEAN) RETURN STD_LOGIC IS
     BEGIN IF arg1 THEN RETURN '1'; ELSE RETURN '0'; END IF; END;
--   FUNCTION TO_STDLOGIC(arg1: STD_ULOGIC_VECTOR(0 DOWNTO 0)) RETURN STD_LOGIC IS
--     BEGIN RETURN arg1(0); END;
   FUNCTION TO_STDLOGIC(arg1: STD_LOGIC_VECTOR(0 DOWNTO 0)) RETURN STD_LOGIC IS
     BEGIN RETURN arg1(0); END;
   FUNCTION TO_STDLOGIC(arg1: UNSIGNED) RETURN STD_LOGIC IS
     BEGIN RETURN arg1(0); END;
   FUNCTION TO_STDLOGIC(arg1: SIGNED(0 DOWNTO 0)) RETURN STD_LOGIC IS
     BEGIN RETURN arg1(0); END;

   FUNCTION TO_STDLOGICVECTOR(arg1: STD_LOGIC) RETURN STD_LOGIC_VECTOR IS
     VARIABLE result: STD_LOGIC_VECTOR(0 DOWNTO 0);
   BEGIN
     result := (0 => arg1);
     RETURN result;
   END;

   FUNCTION maximum (arg1,arg2: INTEGER) RETURN INTEGER IS
   BEGIN
     IF(arg1 > arg2) THEN
       RETURN(arg1) ;
     ELSE
       RETURN(arg2) ;
     END IF;
   END;

   FUNCTION minimum (arg1,arg2: INTEGER) RETURN INTEGER IS
   BEGIN
     IF(arg1 < arg2) THEN
       RETURN(arg1) ;
     ELSE
       RETURN(arg2) ;
     END IF;
   END;

   FUNCTION ifeqsel(arg1, arg2, seleq, selne : INTEGER) RETURN INTEGER IS
   BEGIN
     IF(arg1 = arg2) THEN
       RETURN(seleq) ;
     ELSE
       RETURN(selne) ;
     END IF;
   END;

   FUNCTION resolve_std_logic_vector(input1: STD_LOGIC_VECTOR; input2: STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
     CONSTANT len: INTEGER := input1'LENGTH;
     ALIAS input1a: STD_LOGIC_VECTOR(len-1 DOWNTO 0) IS input1;
     ALIAS input2a: STD_LOGIC_VECTOR(len-1 DOWNTO 0) IS input2;
     VARIABLE result: STD_LOGIC_VECTOR(len-1 DOWNTO 0);
   BEGIN
     result := (others => '0');
     --synopsys translate_off
     FOR i IN len-1 DOWNTO 0 LOOP
       result(i) := resolved(input1a(i) & input2a(i));
     END LOOP;
     --synopsys translate_on
     RETURN result;
   END;

   FUNCTION resolve_unsigned(input1: UNSIGNED; input2: UNSIGNED) RETURN UNSIGNED IS
   BEGIN RETURN UNSIGNED(resolve_std_logic_vector(STD_LOGIC_VECTOR(input1), STD_LOGIC_VECTOR(input2))); END;

   FUNCTION resolve_signed(input1: SIGNED; input2: SIGNED) RETURN SIGNED IS
   BEGIN RETURN SIGNED(resolve_std_logic_vector(STD_LOGIC_VECTOR(input1), STD_LOGIC_VECTOR(input2))); END;

-----------------------------------------------------------------
-- Logic Functions
-----------------------------------------------------------------

   FUNCTION "not"(arg1: UNSIGNED) RETURN UNSIGNED IS
     BEGIN RETURN UNSIGNED(not STD_LOGIC_VECTOR(arg1)); END;
   FUNCTION and_s(inputs: STD_LOGIC_VECTOR) RETURN STD_LOGIC IS
     BEGIN RETURN TO_STDLOGIC(and_v(inputs, 1)); END;
   FUNCTION or_s (inputs: STD_LOGIC_VECTOR) RETURN STD_LOGIC IS
     BEGIN RETURN TO_STDLOGIC(or_v(inputs, 1)); END;
   FUNCTION xor_s(inputs: STD_LOGIC_VECTOR) RETURN STD_LOGIC IS
     BEGIN RETURN TO_STDLOGIC(xor_v(inputs, 1)); END;

   FUNCTION and_v(inputs: STD_LOGIC_VECTOR; olen: POSITIVE) RETURN STD_LOGIC_VECTOR IS
     CONSTANT ilen: POSITIVE := inputs'LENGTH;
     CONSTANT ilenM1: POSITIVE := ilen-1; --2.1.6.3
     CONSTANT olenM1: INTEGER := olen-1; --2.1.6.3
     CONSTANT ilenMolenM1: INTEGER := ilen-olen-1; --2.1.6.3
     VARIABLE inputsx: STD_LOGIC_VECTOR(ilen-1 DOWNTO 0);
     CONSTANT icnt2: POSITIVE:= inputs'LENGTH/olen;
     VARIABLE result: STD_LOGIC_VECTOR(olen-1 DOWNTO 0);
   BEGIN
     --synopsys translate_off
     ASSERT ilen REM olen = 0 SEVERITY FAILURE;
     --synopsys translate_on
     inputsx := inputs;
     result := inputsx(olenM1 DOWNTO 0);
     FOR i IN icnt2-1 DOWNTO 1 LOOP
       inputsx(ilenMolenM1 DOWNTO 0) := inputsx(ilenM1 DOWNTO olen);
       result := result AND inputsx(olenM1 DOWNTO 0);
     END LOOP;
     RETURN result;
   END;

   FUNCTION or_v(inputs: STD_LOGIC_VECTOR; olen: POSITIVE) RETURN STD_LOGIC_VECTOR IS
     CONSTANT ilen: POSITIVE := inputs'LENGTH;
     CONSTANT ilenM1: POSITIVE := ilen-1; --2.1.6.3
     CONSTANT olenM1: INTEGER := olen-1; --2.1.6.3
     CONSTANT ilenMolenM1: INTEGER := ilen-olen-1; --2.1.6.3
     VARIABLE inputsx: STD_LOGIC_VECTOR(ilen-1 DOWNTO 0);
     CONSTANT icnt2: POSITIVE:= inputs'LENGTH/olen;
     VARIABLE result: STD_LOGIC_VECTOR(olen-1 DOWNTO 0);
   BEGIN
     --synopsys translate_off
     ASSERT ilen REM olen = 0 SEVERITY FAILURE;
     --synopsys translate_on
     inputsx := inputs;
     result := inputsx(olenM1 DOWNTO 0);
     -- this if is added as a quick fix for a bug in catapult evaluating the loop even if inputs'LENGTH==1
     -- see dts0100971279
     IF icnt2 > 1 THEN
       FOR i IN icnt2-1 DOWNTO 1 LOOP
         inputsx(ilenMolenM1 DOWNTO 0) := inputsx(ilenM1 DOWNTO olen);
         result := result OR inputsx(olenM1 DOWNTO 0);
       END LOOP;
     END IF;
     RETURN result;
   END;

   FUNCTION xor_v(inputs: STD_LOGIC_VECTOR; olen: POSITIVE) RETURN STD_LOGIC_VECTOR IS
     CONSTANT ilen: POSITIVE := inputs'LENGTH;
     CONSTANT ilenM1: POSITIVE := ilen-1; --2.1.6.3
     CONSTANT olenM1: INTEGER := olen-1; --2.1.6.3
     CONSTANT ilenMolenM1: INTEGER := ilen-olen-1; --2.1.6.3
     VARIABLE inputsx: STD_LOGIC_VECTOR(ilen-1 DOWNTO 0);
     CONSTANT icnt2: POSITIVE:= inputs'LENGTH/olen;
     VARIABLE result: STD_LOGIC_VECTOR(olen-1 DOWNTO 0);
   BEGIN
     --synopsys translate_off
     ASSERT ilen REM olen = 0 SEVERITY FAILURE;
     --synopsys translate_on
     inputsx := inputs;
     result := inputsx(olenM1 DOWNTO 0);
     FOR i IN icnt2-1 DOWNTO 1 LOOP
       inputsx(ilenMolenM1 DOWNTO 0) := inputsx(ilenM1 DOWNTO olen);
       result := result XOR inputsx(olenM1 DOWNTO 0);
     END LOOP;
     RETURN result;
   END;

-----------------------------------------------------------------
-- Muxes
-----------------------------------------------------------------
   
   FUNCTION mux_sel2_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR(1 DOWNTO 0))
   RETURN STD_LOGIC_VECTOR IS
     CONSTANT size   : POSITIVE := inputs'LENGTH / 4;
     ALIAS    inputs0: STD_LOGIC_VECTOR( inputs'LENGTH-1 DOWNTO 0) IS inputs;
     VARIABLE result : STD_LOGIC_Vector( size-1 DOWNTO 0);
   BEGIN
     -- for synthesis only
     -- simulation inconsistent with control values 'UXZHLWD'
     CASE sel IS
     WHEN "00" =>
       result := inputs0(1*size-1 DOWNTO 0*size);
     WHEN "01" =>
       result := inputs0(2*size-1 DOWNTO 1*size);
     WHEN "10" =>
       result := inputs0(3*size-1 DOWNTO 2*size);
     WHEN "11" =>
       result := inputs0(4*size-1 DOWNTO 3*size);
     WHEN others =>
       result := (others => 'X');
     END CASE;
     RETURN result;
   END;
   
   FUNCTION mux_sel3_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR(2 DOWNTO 0))
   RETURN STD_LOGIC_VECTOR IS
     CONSTANT size   : POSITIVE := inputs'LENGTH / 8;
     ALIAS    inputs0: STD_LOGIC_VECTOR(inputs'LENGTH-1 DOWNTO 0) IS inputs;
     VARIABLE result : STD_LOGIC_Vector(size-1 DOWNTO 0);
   BEGIN
     -- for synthesis only
     -- simulation inconsistent with control values 'UXZHLWD'
     CASE sel IS
     WHEN "000" =>
       result := inputs0(1*size-1 DOWNTO 0*size);
     WHEN "001" =>
       result := inputs0(2*size-1 DOWNTO 1*size);
     WHEN "010" =>
       result := inputs0(3*size-1 DOWNTO 2*size);
     WHEN "011" =>
       result := inputs0(4*size-1 DOWNTO 3*size);
     WHEN "100" =>
       result := inputs0(5*size-1 DOWNTO 4*size);
     WHEN "101" =>
       result := inputs0(6*size-1 DOWNTO 5*size);
     WHEN "110" =>
       result := inputs0(7*size-1 DOWNTO 6*size);
     WHEN "111" =>
       result := inputs0(8*size-1 DOWNTO 7*size);
     WHEN others =>
       result := (others => 'X');
     END CASE;
     RETURN result;
   END;
   
   FUNCTION mux_sel4_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR(3 DOWNTO 0))
   RETURN STD_LOGIC_VECTOR IS
     CONSTANT size   : POSITIVE := inputs'LENGTH / 16;
     ALIAS    inputs0: STD_LOGIC_VECTOR(inputs'LENGTH-1 DOWNTO 0) IS inputs;
     VARIABLE result : STD_LOGIC_Vector(size-1 DOWNTO 0);
   BEGIN
     -- for synthesis only
     -- simulation inconsistent with control values 'UXZHLWD'
     CASE sel IS
     WHEN "0000" =>
       result := inputs0( 1*size-1 DOWNTO 0*size);
     WHEN "0001" =>
       result := inputs0( 2*size-1 DOWNTO 1*size);
     WHEN "0010" =>
       result := inputs0( 3*size-1 DOWNTO 2*size);
     WHEN "0011" =>
       result := inputs0( 4*size-1 DOWNTO 3*size);
     WHEN "0100" =>
       result := inputs0( 5*size-1 DOWNTO 4*size);
     WHEN "0101" =>
       result := inputs0( 6*size-1 DOWNTO 5*size);
     WHEN "0110" =>
       result := inputs0( 7*size-1 DOWNTO 6*size);
     WHEN "0111" =>
       result := inputs0( 8*size-1 DOWNTO 7*size);
     WHEN "1000" =>
       result := inputs0( 9*size-1 DOWNTO 8*size);
     WHEN "1001" =>
       result := inputs0( 10*size-1 DOWNTO 9*size);
     WHEN "1010" =>
       result := inputs0( 11*size-1 DOWNTO 10*size);
     WHEN "1011" =>
       result := inputs0( 12*size-1 DOWNTO 11*size);
     WHEN "1100" =>
       result := inputs0( 13*size-1 DOWNTO 12*size);
     WHEN "1101" =>
       result := inputs0( 14*size-1 DOWNTO 13*size);
     WHEN "1110" =>
       result := inputs0( 15*size-1 DOWNTO 14*size);
     WHEN "1111" =>
       result := inputs0( 16*size-1 DOWNTO 15*size);
     WHEN others =>
       result := (others => 'X');
     END CASE;
     RETURN result;
   END;
   
   FUNCTION mux_sel5_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR(4 DOWNTO 0))
   RETURN STD_LOGIC_VECTOR IS
     CONSTANT size   : POSITIVE := inputs'LENGTH / 32;
     ALIAS    inputs0: STD_LOGIC_VECTOR(inputs'LENGTH-1 DOWNTO 0) IS inputs;
     VARIABLE result : STD_LOGIC_Vector(size-1 DOWNTO 0 );
   BEGIN
     -- for synthesis only
     -- simulation inconsistent with control values 'UXZHLWD'
     CASE sel IS
     WHEN "00000" =>
       result := inputs0( 1*size-1 DOWNTO 0*size);
     WHEN "00001" =>
       result := inputs0( 2*size-1 DOWNTO 1*size);
     WHEN "00010" =>
       result := inputs0( 3*size-1 DOWNTO 2*size);
     WHEN "00011" =>
       result := inputs0( 4*size-1 DOWNTO 3*size);
     WHEN "00100" =>
       result := inputs0( 5*size-1 DOWNTO 4*size);
     WHEN "00101" =>
       result := inputs0( 6*size-1 DOWNTO 5*size);
     WHEN "00110" =>
       result := inputs0( 7*size-1 DOWNTO 6*size);
     WHEN "00111" =>
       result := inputs0( 8*size-1 DOWNTO 7*size);
     WHEN "01000" =>
       result := inputs0( 9*size-1 DOWNTO 8*size);
     WHEN "01001" =>
       result := inputs0( 10*size-1 DOWNTO 9*size);
     WHEN "01010" =>
       result := inputs0( 11*size-1 DOWNTO 10*size);
     WHEN "01011" =>
       result := inputs0( 12*size-1 DOWNTO 11*size);
     WHEN "01100" =>
       result := inputs0( 13*size-1 DOWNTO 12*size);
     WHEN "01101" =>
       result := inputs0( 14*size-1 DOWNTO 13*size);
     WHEN "01110" =>
       result := inputs0( 15*size-1 DOWNTO 14*size);
     WHEN "01111" =>
       result := inputs0( 16*size-1 DOWNTO 15*size);
     WHEN "10000" =>
       result := inputs0( 17*size-1 DOWNTO 16*size);
     WHEN "10001" =>
       result := inputs0( 18*size-1 DOWNTO 17*size);
     WHEN "10010" =>
       result := inputs0( 19*size-1 DOWNTO 18*size);
     WHEN "10011" =>
       result := inputs0( 20*size-1 DOWNTO 19*size);
     WHEN "10100" =>
       result := inputs0( 21*size-1 DOWNTO 20*size);
     WHEN "10101" =>
       result := inputs0( 22*size-1 DOWNTO 21*size);
     WHEN "10110" =>
       result := inputs0( 23*size-1 DOWNTO 22*size);
     WHEN "10111" =>
       result := inputs0( 24*size-1 DOWNTO 23*size);
     WHEN "11000" =>
       result := inputs0( 25*size-1 DOWNTO 24*size);
     WHEN "11001" =>
       result := inputs0( 26*size-1 DOWNTO 25*size);
     WHEN "11010" =>
       result := inputs0( 27*size-1 DOWNTO 26*size);
     WHEN "11011" =>
       result := inputs0( 28*size-1 DOWNTO 27*size);
     WHEN "11100" =>
       result := inputs0( 29*size-1 DOWNTO 28*size);
     WHEN "11101" =>
       result := inputs0( 30*size-1 DOWNTO 29*size);
     WHEN "11110" =>
       result := inputs0( 31*size-1 DOWNTO 30*size);
     WHEN "11111" =>
       result := inputs0( 32*size-1 DOWNTO 31*size);
     WHEN others =>
       result := (others => 'X');
     END CASE;
     RETURN result;
   END;
   
   FUNCTION mux_sel6_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR(5 DOWNTO 0))
   RETURN STD_LOGIC_VECTOR IS
     CONSTANT size   : POSITIVE := inputs'LENGTH / 64;
     ALIAS    inputs0: STD_LOGIC_VECTOR(inputs'LENGTH-1 DOWNTO 0) IS inputs;
     VARIABLE result : STD_LOGIC_Vector(size-1 DOWNTO 0);
   BEGIN
     -- for synthesis only
     -- simulation inconsistent with control values 'UXZHLWD'
     CASE sel IS
     WHEN "000000" =>
       result := inputs0( 1*size-1 DOWNTO 0*size);
     WHEN "000001" =>
       result := inputs0( 2*size-1 DOWNTO 1*size);
     WHEN "000010" =>
       result := inputs0( 3*size-1 DOWNTO 2*size);
     WHEN "000011" =>
       result := inputs0( 4*size-1 DOWNTO 3*size);
     WHEN "000100" =>
       result := inputs0( 5*size-1 DOWNTO 4*size);
     WHEN "000101" =>
       result := inputs0( 6*size-1 DOWNTO 5*size);
     WHEN "000110" =>
       result := inputs0( 7*size-1 DOWNTO 6*size);
     WHEN "000111" =>
       result := inputs0( 8*size-1 DOWNTO 7*size);
     WHEN "001000" =>
       result := inputs0( 9*size-1 DOWNTO 8*size);
     WHEN "001001" =>
       result := inputs0( 10*size-1 DOWNTO 9*size);
     WHEN "001010" =>
       result := inputs0( 11*size-1 DOWNTO 10*size);
     WHEN "001011" =>
       result := inputs0( 12*size-1 DOWNTO 11*size);
     WHEN "001100" =>
       result := inputs0( 13*size-1 DOWNTO 12*size);
     WHEN "001101" =>
       result := inputs0( 14*size-1 DOWNTO 13*size);
     WHEN "001110" =>
       result := inputs0( 15*size-1 DOWNTO 14*size);
     WHEN "001111" =>
       result := inputs0( 16*size-1 DOWNTO 15*size);
     WHEN "010000" =>
       result := inputs0( 17*size-1 DOWNTO 16*size);
     WHEN "010001" =>
       result := inputs0( 18*size-1 DOWNTO 17*size);
     WHEN "010010" =>
       result := inputs0( 19*size-1 DOWNTO 18*size);
     WHEN "010011" =>
       result := inputs0( 20*size-1 DOWNTO 19*size);
     WHEN "010100" =>
       result := inputs0( 21*size-1 DOWNTO 20*size);
     WHEN "010101" =>
       result := inputs0( 22*size-1 DOWNTO 21*size);
     WHEN "010110" =>
       result := inputs0( 23*size-1 DOWNTO 22*size);
     WHEN "010111" =>
       result := inputs0( 24*size-1 DOWNTO 23*size);
     WHEN "011000" =>
       result := inputs0( 25*size-1 DOWNTO 24*size);
     WHEN "011001" =>
       result := inputs0( 26*size-1 DOWNTO 25*size);
     WHEN "011010" =>
       result := inputs0( 27*size-1 DOWNTO 26*size);
     WHEN "011011" =>
       result := inputs0( 28*size-1 DOWNTO 27*size);
     WHEN "011100" =>
       result := inputs0( 29*size-1 DOWNTO 28*size);
     WHEN "011101" =>
       result := inputs0( 30*size-1 DOWNTO 29*size);
     WHEN "011110" =>
       result := inputs0( 31*size-1 DOWNTO 30*size);
     WHEN "011111" =>
       result := inputs0( 32*size-1 DOWNTO 31*size);
     WHEN "100000" =>
       result := inputs0( 33*size-1 DOWNTO 32*size);
     WHEN "100001" =>
       result := inputs0( 34*size-1 DOWNTO 33*size);
     WHEN "100010" =>
       result := inputs0( 35*size-1 DOWNTO 34*size);
     WHEN "100011" =>
       result := inputs0( 36*size-1 DOWNTO 35*size);
     WHEN "100100" =>
       result := inputs0( 37*size-1 DOWNTO 36*size);
     WHEN "100101" =>
       result := inputs0( 38*size-1 DOWNTO 37*size);
     WHEN "100110" =>
       result := inputs0( 39*size-1 DOWNTO 38*size);
     WHEN "100111" =>
       result := inputs0( 40*size-1 DOWNTO 39*size);
     WHEN "101000" =>
       result := inputs0( 41*size-1 DOWNTO 40*size);
     WHEN "101001" =>
       result := inputs0( 42*size-1 DOWNTO 41*size);
     WHEN "101010" =>
       result := inputs0( 43*size-1 DOWNTO 42*size);
     WHEN "101011" =>
       result := inputs0( 44*size-1 DOWNTO 43*size);
     WHEN "101100" =>
       result := inputs0( 45*size-1 DOWNTO 44*size);
     WHEN "101101" =>
       result := inputs0( 46*size-1 DOWNTO 45*size);
     WHEN "101110" =>
       result := inputs0( 47*size-1 DOWNTO 46*size);
     WHEN "101111" =>
       result := inputs0( 48*size-1 DOWNTO 47*size);
     WHEN "110000" =>
       result := inputs0( 49*size-1 DOWNTO 48*size);
     WHEN "110001" =>
       result := inputs0( 50*size-1 DOWNTO 49*size);
     WHEN "110010" =>
       result := inputs0( 51*size-1 DOWNTO 50*size);
     WHEN "110011" =>
       result := inputs0( 52*size-1 DOWNTO 51*size);
     WHEN "110100" =>
       result := inputs0( 53*size-1 DOWNTO 52*size);
     WHEN "110101" =>
       result := inputs0( 54*size-1 DOWNTO 53*size);
     WHEN "110110" =>
       result := inputs0( 55*size-1 DOWNTO 54*size);
     WHEN "110111" =>
       result := inputs0( 56*size-1 DOWNTO 55*size);
     WHEN "111000" =>
       result := inputs0( 57*size-1 DOWNTO 56*size);
     WHEN "111001" =>
       result := inputs0( 58*size-1 DOWNTO 57*size);
     WHEN "111010" =>
       result := inputs0( 59*size-1 DOWNTO 58*size);
     WHEN "111011" =>
       result := inputs0( 60*size-1 DOWNTO 59*size);
     WHEN "111100" =>
       result := inputs0( 61*size-1 DOWNTO 60*size);
     WHEN "111101" =>
       result := inputs0( 62*size-1 DOWNTO 61*size);
     WHEN "111110" =>
       result := inputs0( 63*size-1 DOWNTO 62*size);
     WHEN "111111" =>
       result := inputs0( 64*size-1 DOWNTO 63*size);
     WHEN others =>
       result := (others => 'X');
     END CASE;
     RETURN result;
   END;

   FUNCTION mux_s(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC) RETURN STD_LOGIC IS
   BEGIN RETURN TO_STDLOGIC(mux_v(inputs, sel)); END;

   FUNCTION mux_s(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC_VECTOR) RETURN STD_LOGIC IS
   BEGIN RETURN TO_STDLOGIC(mux_v(inputs, sel)); END;

   FUNCTION mux_v(inputs: STD_LOGIC_VECTOR; sel: STD_LOGIC) RETURN STD_LOGIC_VECTOR IS  --pragma hls_map_to_operator mux
     ALIAS    inputs0: STD_LOGIC_VECTOR(inputs'LENGTH-1 DOWNTO 0) IS inputs;
     CONSTANT size   : POSITIVE := inputs'LENGTH / 2;
     CONSTANT olen   : POSITIVE := inputs'LENGTH / 2;
     VARIABLE result : STD_LOGIC_VECTOR(olen-1 DOWNTO 0);
   BEGIN
     --synopsys translate_off
     ASSERT inputs'LENGTH = olen * 2 SEVERITY FAILURE;
     --synopsys translate_on
       CASE sel IS
       WHEN '1'
     --synopsys translate_off
            | 'H'
     --synopsys translate_on
            =>
         result := inputs0( size-1 DOWNTO 0);
       WHEN '0' 
     --synopsys translate_off
            | 'L'
     --synopsys translate_on
            =>
         result := inputs0(2*size-1  DOWNTO size);
       WHEN others =>
         --synopsys translate_off
         result := resolve_std_logic_vector(inputs0(size-1 DOWNTO 0), inputs0( 2*size-1 DOWNTO size));
         --synopsys translate_on
       END CASE;
       RETURN result;
   END;
--   BEGIN RETURN mux_v(inputs, TO_STDLOGICVECTOR(sel)); END;

   FUNCTION mux_v(inputs: STD_LOGIC_VECTOR; sel : STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS --pragma hls_map_to_operator mux
     ALIAS    inputs0: STD_LOGIC_VECTOR( inputs'LENGTH-1 DOWNTO 0) IS inputs;
     ALIAS    sel0   : STD_LOGIC_VECTOR( sel'LENGTH-1 DOWNTO 0 ) IS sel;

     VARIABLE sellen : INTEGER RANGE 2-sel'LENGTH TO sel'LENGTH;
     CONSTANT size   : POSITIVE := inputs'LENGTH / 2;
     CONSTANT olen   : POSITIVE := inputs'LENGTH / 2**sel'LENGTH;
     VARIABLE result : STD_LOGIC_VECTOR(olen-1 DOWNTO 0);
     TYPE inputs_array_type is array(natural range <>) of std_logic_vector( olen - 1 DOWNTO 0);
     VARIABLE inputs_array : inputs_array_type( 2**sel'LENGTH - 1 DOWNTO 0);
   BEGIN
     sellen := sel'LENGTH;
     --synopsys translate_off
     ASSERT inputs'LENGTH = olen * 2**sellen SEVERITY FAILURE;
     sellen := 2-sellen;
     --synopsys translate_on
     CASE sellen IS
     WHEN 1 =>
       CASE sel0(0) IS

       WHEN '1' 
     --synopsys translate_off
            | 'H'
     --synopsys translate_on
            =>
         result := inputs0(  size-1 DOWNTO 0);
       WHEN '0' 
     --synopsys translate_off
            | 'L'
     --synopsys translate_on
            =>
         result := inputs0(2*size-1 DOWNTO size);
       WHEN others =>
         --synopsys translate_off
         result := resolve_std_logic_vector(inputs0( size-1 DOWNTO 0), inputs0( 2*size-1 DOWNTO size));
         --synopsys translate_on
       END CASE;
     WHEN 2 =>
       result := mux_sel2_v(inputs, not sel);
     WHEN 3 =>
       result := mux_sel3_v(inputs, not sel);
     WHEN 4 =>
       result := mux_sel4_v(inputs, not sel);
     WHEN 5 =>
       result := mux_sel5_v(inputs, not sel);
     WHEN 6 =>
       result := mux_sel6_v(inputs, not sel);
     WHEN others =>
       -- synopsys translate_off
       IF(Is_X(sel0)) THEN
         result := (others => 'X');
       ELSE
       -- synopsys translate_on
         FOR i in 0 to 2**sel'LENGTH - 1 LOOP
           inputs_array(i) := inputs0( ((i + 1) * olen) - 1  DOWNTO i*olen);
         END LOOP;
         result := inputs_array(CONV_INTEGER( (UNSIGNED(NOT sel0)) ));
       -- synopsys translate_off
       END IF;
       -- synopsys translate_on
     END CASE;
     RETURN result;
   END;

 
-----------------------------------------------------------------
-- Latches
-----------------------------------------------------------------

   FUNCTION lat_s(dinput: STD_LOGIC; clk: STD_LOGIC; doutput: STD_LOGIC) RETURN STD_LOGIC IS
   BEGIN RETURN mux_s(STD_LOGIC_VECTOR'(doutput & dinput), clk); END;

   FUNCTION lat_v(dinput: STD_LOGIC_VECTOR ; clk: STD_LOGIC; doutput: STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR IS
   BEGIN
     --synopsys translate_off
     ASSERT dinput'LENGTH = doutput'LENGTH SEVERITY FAILURE;
     --synopsys translate_on
     RETURN mux_v(doutput & dinput, clk);
   END;

-----------------------------------------------------------------
-- Tri-States
-----------------------------------------------------------------
--   FUNCTION tri_s(dinput: STD_LOGIC; control: STD_LOGIC) RETURN STD_LOGIC IS
--   BEGIN RETURN TO_STDLOGIC(tri_v(TO_STDLOGICVECTOR(dinput), control)); END;
--
--   FUNCTION tri_v(dinput: STD_LOGIC_VECTOR ; control: STD_LOGIC) RETURN STD_LOGIC_VECTOR IS
--     VARIABLE result: STD_LOGIC_VECTOR(dinput'range);
--   BEGIN
--     CASE control IS
--     WHEN '0' | 'L' =>
--       result := (others => 'Z');
--     WHEN '1' | 'H' =>
--       FOR i IN dinput'range LOOP
--         result(i) := to_UX01(dinput(i));
--       END LOOP;
--     WHEN others =>
--       -- synopsys translate_off
--       result := (others => 'X');
--       -- synopsys translate_on
--     END CASE;
--     RETURN result;
--   END;

-----------------------------------------------------------------
-- compare functions returning STD_LOGIC
-- in contrast to the functions returning boolean
-----------------------------------------------------------------

   FUNCTION "=" (l, r: UNSIGNED) RETURN STD_LOGIC IS
     BEGIN RETURN not or_s(STD_LOGIC_VECTOR(l) xor STD_LOGIC_VECTOR(r)); END;
   FUNCTION "=" (l, r: SIGNED  ) RETURN STD_LOGIC IS
     BEGIN RETURN not or_s(STD_LOGIC_VECTOR(l) xor STD_LOGIC_VECTOR(r)); END;
   FUNCTION "/="(l, r: UNSIGNED) RETURN STD_LOGIC IS
     BEGIN RETURN or_s(STD_LOGIC_VECTOR(l) xor STD_LOGIC_VECTOR(r)); END;
   FUNCTION "/="(l, r: SIGNED  ) RETURN STD_LOGIC IS
     BEGIN RETURN or_s(STD_LOGIC_VECTOR(l) xor STD_LOGIC_VECTOR(r)); END;

   FUNCTION "<" (l, r: UNSIGNED) RETURN STD_LOGIC IS
     VARIABLE diff: UNSIGNED(l'LENGTH DOWNTO 0);
   BEGIN
     --synopsys translate_off
     ASSERT l'LENGTH = r'LENGTH SEVERITY FAILURE;
     --synopsys translate_on
     diff := ('0'&l) - ('0'&r);
     RETURN diff(l'LENGTH);
   END;
   FUNCTION "<"(l, r: SIGNED  ) RETURN STD_LOGIC IS
   BEGIN
     RETURN (UNSIGNED(l) < UNSIGNED(r)) xor (l(l'LEFT) xor r(r'LEFT));
   END;

   FUNCTION "<="(l, r: UNSIGNED) RETURN STD_LOGIC IS
     BEGIN RETURN not STD_LOGIC'(r < l); END;
   FUNCTION "<=" (l, r: SIGNED  ) RETURN STD_LOGIC IS
     BEGIN RETURN not STD_LOGIC'(r < l); END;
   FUNCTION ">" (l, r: UNSIGNED) RETURN STD_LOGIC IS
     BEGIN RETURN r < l; END;
   FUNCTION ">"(l, r: SIGNED  ) RETURN STD_LOGIC IS
     BEGIN RETURN r < l; END;
   FUNCTION ">="(l, r: UNSIGNED) RETURN STD_LOGIC IS
     BEGIN RETURN not STD_LOGIC'(l < r); END;
   FUNCTION ">=" (l, r: SIGNED  ) RETURN STD_LOGIC IS
     BEGIN RETURN not STD_LOGIC'(l < r); END;

   FUNCTION cmp (l, r: STD_LOGIC_VECTOR) RETURN STD_LOGIC IS
   BEGIN
     --synopsys translate_off
     ASSERT l'LENGTH = r'LENGTH SEVERITY FAILURE;
     --synopsys translate_on
     RETURN not or_s(l xor r);
   END;

-----------------------------------------------------------------
-- Vectorized Overloaded Arithmetic Operators
-----------------------------------------------------------------

   --some functions to placate spyglass
   FUNCTION mult_natural(a,b : NATURAL) RETURN NATURAL IS
   BEGIN
     return a*b;
   END mult_natural;

   FUNCTION div_natural(a,b : NATURAL) RETURN NATURAL IS
   BEGIN
     return a/b;
   END div_natural;

   FUNCTION mod_natural(a,b : NATURAL) RETURN NATURAL IS
   BEGIN
     return a mod b;
   END mod_natural;

   FUNCTION add_unsigned(a,b : UNSIGNED) RETURN UNSIGNED IS
   BEGIN
     return a+b;
   END add_unsigned;

   FUNCTION sub_unsigned(a,b : UNSIGNED) RETURN UNSIGNED IS
   BEGIN
     return a-b;
   END sub_unsigned;

   FUNCTION sub_int(a,b : INTEGER) RETURN INTEGER IS
   BEGIN
     return a-b;
   END sub_int;

   FUNCTION concat_0(b : UNSIGNED) RETURN UNSIGNED IS
   BEGIN
     return '0' & b;
   END concat_0;

   FUNCTION concat_uns(a,b : UNSIGNED) RETURN UNSIGNED IS
   BEGIN
     return a&b;
   END concat_uns;

   FUNCTION concat_vect(a,b : STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
   BEGIN
     return a&b;
   END concat_vect;





   FUNCTION faccu(arg: UNSIGNED; width: NATURAL) RETURN UNSIGNED IS
     CONSTANT ninps : NATURAL := arg'LENGTH / width;
     ALIAS    arg0  : UNSIGNED(arg'LENGTH-1 DOWNTO 0) IS arg;
     VARIABLE result: UNSIGNED(width-1 DOWNTO 0);
     VARIABLE from  : INTEGER;
     VARIABLE dto   : INTEGER;
   BEGIN
     --synopsys translate_off
     ASSERT arg'LENGTH = width * ninps SEVERITY FAILURE;
     --synopsys translate_on
     result := (OTHERS => '0');
     FOR i IN ninps-1 DOWNTO 0 LOOP
       --result := result + arg0((i+1)*width-1 DOWNTO i*width);
       from := mult_natural((i+1), width)-1; --2.1.6.3
       dto  := mult_natural(i,width); --2.1.6.3
       result := add_unsigned(result , arg0(from DOWNTO dto) );
     END LOOP;
     RETURN result;
   END faccu;

   FUNCTION  fabs (arg1: SIGNED) RETURN UNSIGNED IS
   BEGIN
     CASE arg1(arg1'LEFT) IS
     WHEN '1'
     --synopsys translate_off
          | 'H'
     --synopsys translate_on
       =>
       RETURN UNSIGNED'("0") - UNSIGNED(arg1);
     WHEN '0'
     --synopsys translate_off
          | 'L'
     --synopsys translate_on
       =>
       RETURN UNSIGNED(arg1);
     WHEN others =>
       RETURN resolve_unsigned(UNSIGNED(arg1), UNSIGNED'("0") - UNSIGNED(arg1));
     END CASE;
   END;

   PROCEDURE divmod(l, r: UNSIGNED; rdiv, rmod: OUT UNSIGNED) IS
     CONSTANT llen: INTEGER := l'LENGTH;
     CONSTANT rlen: INTEGER := r'LENGTH;
     CONSTANT llen_plus_rlen: INTEGER := llen + rlen;
     VARIABLE lbuf: UNSIGNED(llen+rlen-1 DOWNTO 0);
     VARIABLE diff: UNSIGNED(rlen DOWNTO 0);
   BEGIN
     --synopsys translate_off
     ASSERT rdiv'LENGTH = llen AND rmod'LENGTH = rlen SEVERITY FAILURE;
     --synopsys translate_on
     lbuf := (others => '0');
     lbuf(llen-1 DOWNTO 0) := l;
     FOR i IN rdiv'range LOOP
       diff := sub_unsigned(lbuf(llen_plus_rlen-1 DOWNTO llen-1) ,(concat_0(r)));
       rdiv(i) := not diff(rlen);
       IF diff(rlen) = '0' THEN
         lbuf(llen_plus_rlen-1 DOWNTO llen-1) := diff;
       END IF;
       lbuf(llen_plus_rlen-1 DOWNTO 1) := lbuf(llen_plus_rlen-2 DOWNTO 0);
     END LOOP;
     rmod := lbuf(llen_plus_rlen-1 DOWNTO llen);
   END divmod;

   FUNCTION "/"  (l, r: UNSIGNED) RETURN UNSIGNED IS
     VARIABLE rdiv: UNSIGNED(l'LENGTH-1 DOWNTO 0);
     VARIABLE rmod: UNSIGNED(r'LENGTH-1 DOWNTO 0);
   BEGIN
     divmod(l, r, rdiv, rmod);
     RETURN rdiv;
   END "/";

   FUNCTION "MOD"(l, r: UNSIGNED) RETURN UNSIGNED IS
     VARIABLE rdiv: UNSIGNED(l'LENGTH-1 DOWNTO 0);
     VARIABLE rmod: UNSIGNED(r'LENGTH-1 DOWNTO 0);
   BEGIN
     divmod(l, r, rdiv, rmod);
     RETURN rmod;
   END;

   FUNCTION "REM"(l, r: UNSIGNED) RETURN UNSIGNED IS
     BEGIN RETURN l MOD r; END;

   FUNCTION "/"  (l, r: SIGNED  ) RETURN SIGNED  IS
     VARIABLE rdiv: UNSIGNED(l'LENGTH-1 DOWNTO 0);
     VARIABLE rmod: UNSIGNED(r'LENGTH-1 DOWNTO 0);
   BEGIN
     divmod(fabs(l), fabs(r), rdiv, rmod);
     IF to_X01(l(l'LEFT)) /= to_X01(r(r'LEFT)) THEN
       rdiv := UNSIGNED'("0") - rdiv;
     END IF;
     RETURN SIGNED(rdiv); -- overflow problem "1000" / "11"
   END "/";

   FUNCTION "MOD"(l, r: SIGNED  ) RETURN SIGNED  IS
     VARIABLE rdiv: UNSIGNED(l'LENGTH-1 DOWNTO 0);
     VARIABLE rmod: UNSIGNED(r'LENGTH-1 DOWNTO 0);
     CONSTANT rnul: UNSIGNED(r'LENGTH-1 DOWNTO 0) := (others => '0');
   BEGIN
     divmod(fabs(l), fabs(r), rdiv, rmod);
     IF to_X01(l(l'LEFT)) = '1' THEN
       rmod := UNSIGNED'("0") - rmod;
     END IF;
     IF rmod /= rnul AND to_X01(l(l'LEFT)) /= to_X01(r(r'LEFT)) THEN
       rmod := UNSIGNED(r) + rmod;
     END IF;
     RETURN SIGNED(rmod);
   END "MOD";

   FUNCTION "REM"(l, r: SIGNED  ) RETURN SIGNED  IS
     VARIABLE rdiv: UNSIGNED(l'LENGTH-1 DOWNTO 0);
     VARIABLE rmod: UNSIGNED(r'LENGTH-1 DOWNTO 0);
   BEGIN
     divmod(fabs(l), fabs(r), rdiv, rmod);
     IF to_X01(l(l'LEFT)) = '1' THEN
       rmod := UNSIGNED'("0") - rmod;
     END IF;
     RETURN SIGNED(rmod);
   END "REM";

   FUNCTION mult_unsigned(l,r : UNSIGNED) return UNSIGNED is
   BEGIN
     return l*r; 
   END mult_unsigned;

   FUNCTION "**" (l, r : UNSIGNED) RETURN UNSIGNED IS
     CONSTANT llen  : NATURAL := l'LENGTH;
     VARIABLE result: UNSIGNED(llen-1 DOWNTO 0);
     VARIABLE fak   : UNSIGNED(llen-1 DOWNTO 0);
   BEGIN
     fak := l;
     result := (others => '0'); result(0) := '1';
     FOR i IN r'reverse_range LOOP
       --was:result := UNSIGNED(mux_v(STD_LOGIC_VECTOR(result & (result*fak)), r(i)));
       result := UNSIGNED(mux_v(STD_LOGIC_VECTOR( concat_uns(result , mult_unsigned(result,fak) )), r(i)));

       fak := mult_unsigned(fak , fak);
     END LOOP;
     RETURN result;
   END "**";

   FUNCTION "**" (l, r : SIGNED) RETURN SIGNED IS
     CONSTANT rlen  : NATURAL := r'LENGTH;
     ALIAS    r0    : SIGNED(0 TO r'LENGTH-1) IS r;
     VARIABLE result: SIGNED(l'range);
   BEGIN
     CASE r(r'LEFT) IS
     WHEN '0'
   --synopsys translate_off
          | 'L'
   --synopsys translate_on
     =>
       result := SIGNED(UNSIGNED(l) ** UNSIGNED(r0(1 TO r'LENGTH-1)));
     WHEN '1'
   --synopsys translate_off
          | 'H'
   --synopsys translate_on
     =>
       result := (others => '0');
     WHEN others =>
       result := (others => 'X');
     END CASE;
     RETURN result;
   END "**";

-----------------------------------------------------------------
--               S H I F T   F U C T I O N S
-- negative shift shifts the opposite direction
-----------------------------------------------------------------

   FUNCTION add_nat(arg1 : NATURAL; arg2 : NATURAL ) RETURN NATURAL IS
   BEGIN
     return (arg1 + arg2);
   END;
   
--   FUNCTION UNSIGNED_2_BIT_VECTOR(arg1 : NATURAL; arg2 : NATURAL ) RETURN BIT_VECTOR IS
--   BEGIN
--     return (arg1 + arg2);
--   END;
   
   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT ilen: INTEGER := arg1'LENGTH;
     CONSTANT olenM1: INTEGER := olen-1; -- 2.1.6.3
     CONSTANT ilenub: INTEGER := arg1'LENGTH-1;
     CONSTANT len: INTEGER := maximum(ilen, olen);
     VARIABLE result: UNSIGNED(len-1 DOWNTO 0);
   BEGIN
     result := (others => sbit);
     result(ilenub DOWNTO 0) := arg1;
     result := shl(result, arg2);
     RETURN result(olenM1 DOWNTO 0);
   END;

   FUNCTION fshl_stdar(arg1: SIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN SIGNED IS
     CONSTANT ilen: INTEGER := arg1'LENGTH;
     CONSTANT olenM1: INTEGER := olen-1; -- 2.1.6.3
     CONSTANT ilenub: INTEGER := arg1'LENGTH-1;
     CONSTANT len: INTEGER := maximum(ilen, olen);
     VARIABLE result: SIGNED(len-1 DOWNTO 0);
   BEGIN
     result := (others => sbit);
     result(ilenub DOWNTO 0) := arg1;
     result := shl(SIGNED(result), arg2);
     RETURN result(olenM1 DOWNTO 0);
   END;

   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT ilen: INTEGER := arg1'LENGTH;
     CONSTANT olenM1: INTEGER := olen-1; -- 2.1.6.3
     CONSTANT ilenub: INTEGER := arg1'LENGTH-1;
     CONSTANT len: INTEGER := maximum(ilen, olen);
     VARIABLE result: UNSIGNED(len-1 DOWNTO 0);
   BEGIN
     result := (others => sbit);
     result(ilenub DOWNTO 0) := arg1;
     result := shr(result, arg2);
     RETURN result(olenM1 DOWNTO 0);
   END;

   FUNCTION fshr_stdar(arg1: SIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN SIGNED IS
     CONSTANT ilen: INTEGER := arg1'LENGTH;
     CONSTANT olenM1: INTEGER := olen-1; -- 2.1.6.3
     CONSTANT ilenub: INTEGER := arg1'LENGTH-1;
     CONSTANT len: INTEGER := maximum(ilen, olen);
     VARIABLE result: SIGNED(len-1 DOWNTO 0);
   BEGIN
     result := (others => sbit);
     result(ilenub DOWNTO 0) := arg1;
     result := shr(result, arg2);
     RETURN result(olenM1 DOWNTO 0);
   END;

   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT arg1l: NATURAL := arg1'LENGTH - 1;
     ALIAS    arg1x: UNSIGNED(arg1l DOWNTO 0) IS arg1;
     CONSTANT arg2l: NATURAL := arg2'LENGTH - 1;
     ALIAS    arg2x: SIGNED(arg2l DOWNTO 0) IS arg2;
     VARIABLE arg1x_pad: UNSIGNED(arg1l+1 DOWNTO 0);
     VARIABLE result: UNSIGNED(olen-1 DOWNTO 0);
   BEGIN
     result := (others=>'0');
     arg1x_pad(arg1l+1) := sbit;
     arg1x_pad(arg1l downto 0) := arg1x;
     IF arg2l = 0 THEN
       RETURN fshr_stdar(arg1x_pad, UNSIGNED(arg2x), sbit, olen);
     -- ELSIF arg1l = 0 THEN
     --   RETURN fshl(sbit & arg1x, arg2x, sbit, olen);
     ELSE
       CASE arg2x(arg2l) IS
       WHEN '0'
     --synopsys translate_off
            | 'L'
     --synopsys translate_on
       =>
         RETURN fshl_stdar(arg1x_pad, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);
       WHEN '1'
     --synopsys translate_off
            | 'H'
     --synopsys translate_on
       =>
         RETURN fshr_stdar(arg1x_pad(arg1l+1 DOWNTO 1), '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);
       WHEN others =>
         --synopsys translate_off
         result := resolve_unsigned(
           fshl_stdar(arg1x, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit,  olen),
           fshr_stdar(arg1x_pad(arg1l+1 DOWNTO 1), '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen)
         );
         --synopsys translate_on
         RETURN result;
       END CASE;
     END IF;
   END;

   FUNCTION fshl_stdar(arg1: SIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN SIGNED IS
     CONSTANT arg1l: NATURAL := arg1'LENGTH - 1;
     ALIAS    arg1x: SIGNED(arg1l DOWNTO 0) IS arg1;
     CONSTANT arg2l: NATURAL := arg2'LENGTH - 1;
     ALIAS    arg2x: SIGNED(arg2l DOWNTO 0) IS arg2;
     VARIABLE arg1x_pad: SIGNED(arg1l+1 DOWNTO 0);
     VARIABLE result: SIGNED(olen-1 DOWNTO 0);
   BEGIN
     result := (others=>'0');
     arg1x_pad(arg1l+1) := sbit;
     arg1x_pad(arg1l downto 0) := arg1x;
     IF arg2l = 0 THEN
       RETURN fshr_stdar(arg1x_pad, UNSIGNED(arg2x), sbit, olen);
     -- ELSIF arg1l = 0 THEN
     --   RETURN fshl(sbit & arg1x, arg2x, sbit, olen);
     ELSE
       CASE arg2x(arg2l) IS
       WHEN '0'
       --synopsys translate_off
            | 'L'
       --synopsys translate_on
       =>
         RETURN fshl_stdar(arg1x_pad, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);
       WHEN '1'
       --synopsys translate_off
            | 'H'
       --synopsys translate_on
       =>
         RETURN fshr_stdar(arg1x_pad(arg1l+1 DOWNTO 1), '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);
       WHEN others =>
         --synopsys translate_off
         result := resolve_signed(
           fshl_stdar(arg1x, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit,  olen),
           fshr_stdar(arg1x_pad(arg1l+1 DOWNTO 1), '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen)
         );
         --synopsys translate_on
         RETURN result;
       END CASE;
     END IF;
   END;

   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT arg2l: INTEGER := arg2'LENGTH - 1;
     ALIAS    arg2x: SIGNED(arg2l DOWNTO 0) IS arg2;
     VARIABLE result: UNSIGNED(olen-1 DOWNTO 0);
   BEGIN
     result := (others => '0');
     IF arg2l = 0 THEN
       RETURN fshl_stdar(arg1, UNSIGNED(arg2x), olen);
     ELSE
       CASE arg2x(arg2l) IS
       WHEN '0'
       --synopsys translate_off
            | 'L'
       --synopsys translate_on
        =>
         RETURN fshr_stdar(arg1, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);
       WHEN '1'
       --synopsys translate_off
            | 'H'
       --synopsys translate_on
        =>
         RETURN fshl_stdar(arg1 & '0', '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), olen);
       WHEN others =>
         --synopsys translate_off
         result := resolve_unsigned(
           fshr_stdar(arg1, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen),
           fshl_stdar(arg1 & '0', '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), olen)
         );
         --synopsys translate_on
	 return result;
       END CASE;
     END IF;
   END;

   FUNCTION fshr_stdar(arg1: SIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN SIGNED IS
     CONSTANT arg2l: INTEGER := arg2'LENGTH - 1;
     ALIAS    arg2x: SIGNED(arg2l DOWNTO 0) IS arg2;
     VARIABLE result: SIGNED(olen-1 DOWNTO 0);
   BEGIN
     result := (others => '0');
     IF arg2l = 0 THEN
       RETURN fshl_stdar(arg1, UNSIGNED(arg2x), olen);
     ELSE
       CASE arg2x(arg2l) IS
       WHEN '0'
       --synopsys translate_off
            | 'L'
       --synopsys translate_on
       =>
         RETURN fshr_stdar(arg1, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);
       WHEN '1'
       --synopsys translate_off
            | 'H'
       --synopsys translate_on
       =>
         RETURN fshl_stdar(arg1 & '0', '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), olen);
       WHEN others =>
         --synopsys translate_off
         result := resolve_signed(
           fshr_stdar(arg1, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen),
           fshl_stdar(arg1 & '0', '0' & not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), olen)
         );
         --synopsys translate_on
	 return result;
       END CASE;
     END IF;
   END;

   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshl_stdar(arg1, arg2, '0', olen); END;
   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshr_stdar(arg1, arg2, '0', olen); END;
   FUNCTION fshl_stdar(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshl_stdar(arg1, arg2, '0', olen); END;
   FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshr_stdar(arg1, arg2, '0', olen); END;

   FUNCTION fshl_stdar(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN fshl_stdar(arg1, arg2, arg1(arg1'LEFT), olen); END;
   FUNCTION fshr_stdar(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN fshr_stdar(arg1, arg2, arg1(arg1'LEFT), olen); END;
   FUNCTION fshl_stdar(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN fshl_stdar(arg1, arg2, arg1(arg1'LEFT), olen); END;
   FUNCTION fshr_stdar(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN fshr_stdar(arg1, arg2, arg1(arg1'LEFT), olen); END;


   FUNCTION fshl(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT ilen: INTEGER := arg1'LENGTH;
     CONSTANT olenM1: INTEGER := olen-1; --2.1.6.3
     CONSTANT len: INTEGER := maximum(ilen, olen);
     VARIABLE result: UNSIGNED(len-1 DOWNTO 0);
     VARIABLE temp: UNSIGNED(len-1 DOWNTO 0);
     --SUBTYPE  sw_range IS NATURAL range 1 TO len;
     VARIABLE sw: NATURAL range 1 TO len;
     VARIABLE temp_idx : INTEGER; --2.1.6.3
   BEGIN
     sw := 1;
     result := (others => sbit);
     result(ilen-1 DOWNTO 0) := arg1;
     FOR i IN arg2'reverse_range LOOP
       temp := (others => '0');
       FOR i2 IN len-1-sw DOWNTO 0 LOOP
         --was:temp(i2+sw) := result(i2);
         temp_idx := add_nat(i2,sw);
         temp(temp_idx) := result(i2);
       END LOOP;
       result := UNSIGNED(mux_v(STD_LOGIC_VECTOR(concat_uns(result,temp)), arg2(i)));
       sw := minimum(mult_natural(sw,2), len);
     END LOOP;
     RETURN result(olenM1 DOWNTO 0);
   END;

   FUNCTION fshr(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT ilen: INTEGER := arg1'LENGTH;
     CONSTANT olenM1: INTEGER := olen-1; --2.1.6.3
     CONSTANT len: INTEGER := maximum(ilen, olen);
     VARIABLE result: UNSIGNED(len-1 DOWNTO 0);
     VARIABLE temp: UNSIGNED(len-1 DOWNTO 0);
     SUBTYPE  sw_range IS NATURAL range 1 TO len;
     VARIABLE sw: sw_range;
     VARIABLE result_idx : INTEGER; --2.1.6.3
   BEGIN
     sw := 1;
     result := (others => sbit);
     result(ilen-1 DOWNTO 0) := arg1;
     FOR i IN arg2'reverse_range LOOP
       temp := (others => sbit);
       FOR i2 IN len-1-sw DOWNTO 0 LOOP
         -- was: temp(i2) := result(i2+sw);
         result_idx := add_nat(i2,sw);
         temp(i2) := result(result_idx);
       END LOOP;
       result := UNSIGNED(mux_v(STD_LOGIC_VECTOR(concat_uns(result,temp)), arg2(i)));
       sw := minimum(mult_natural(sw,2), len);
     END LOOP;
     RETURN result(olenM1 DOWNTO 0);
   END;

   FUNCTION fshl(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT arg1l: NATURAL := arg1'LENGTH - 1;
     ALIAS    arg1x: UNSIGNED(arg1l DOWNTO 0) IS arg1;
     CONSTANT arg2l: NATURAL := arg2'LENGTH - 1;
     ALIAS    arg2x: SIGNED(arg2l DOWNTO 0) IS arg2;
     VARIABLE arg1x_pad: UNSIGNED(arg1l+1 DOWNTO 0);
     VARIABLE result: UNSIGNED(olen-1 DOWNTO 0);
   BEGIN
     result := (others=>'0');
     arg1x_pad(arg1l+1) := sbit;
     arg1x_pad(arg1l downto 0) := arg1x;
     IF arg2l = 0 THEN
       RETURN fshr(arg1x_pad, UNSIGNED(arg2x), sbit, olen);
     -- ELSIF arg1l = 0 THEN
     --   RETURN fshl(sbit & arg1x, arg2x, sbit, olen);
     ELSE
       CASE arg2x(arg2l) IS
       WHEN '0'
       --synopsys translate_off
            | 'L'
       --synopsys translate_on
       =>
         RETURN fshl(arg1x_pad, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);

       WHEN '1'
       --synopsys translate_off
            | 'H'
       --synopsys translate_on
       =>
         RETURN fshr(arg1x_pad(arg1l+1 DOWNTO 1), not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);

       WHEN others =>
         --synopsys translate_off
         result := resolve_unsigned(
           fshl(arg1x_pad, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit,  olen),
           fshr(arg1x_pad(arg1l+1 DOWNTO 1), not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen)
         );
         --synopsys translate_on
         RETURN result;
       END CASE;
     END IF;
   END;

   FUNCTION fshr(arg1: UNSIGNED; arg2: SIGNED  ; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
     CONSTANT arg2l: INTEGER := arg2'LENGTH - 1;
     ALIAS    arg2x: SIGNED(arg2l DOWNTO 0) IS arg2;
     VARIABLE result: UNSIGNED(olen-1 DOWNTO 0);
   BEGIN
     result := (others => '0');
     IF arg2l = 0 THEN
       RETURN fshl(arg1, UNSIGNED(arg2x), olen);
     ELSE
       CASE arg2x(arg2l) IS
       WHEN '0'
       --synopsys translate_off
            | 'L'
       --synopsys translate_on
       =>
         RETURN fshr(arg1, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen);

       WHEN '1'
       --synopsys translate_off
            | 'H'
       --synopsys translate_on
       =>
         RETURN fshl(arg1 & '0', not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), olen);
       WHEN others =>
         --synopsys translate_off
         result := resolve_unsigned(
           fshr(arg1, UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), sbit, olen),
           fshl(arg1 & '0', not UNSIGNED(arg2x(arg2l-1 DOWNTO 0)), olen)
         );
         --synopsys translate_on
	 return result;
       END CASE;
     END IF;
   END;

   FUNCTION fshl(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshl(arg1, arg2, '0', olen); END;
   FUNCTION fshr(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshr(arg1, arg2, '0', olen); END;
   FUNCTION fshl(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshl(arg1, arg2, '0', olen); END;
   FUNCTION fshr(arg1: UNSIGNED; arg2: SIGNED  ; olen: POSITIVE) RETURN UNSIGNED IS
     BEGIN RETURN fshr(arg1, arg2, '0', olen); END;

   FUNCTION fshl(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN SIGNED(fshl(UNSIGNED(arg1), arg2, arg1(arg1'LEFT), olen)); END;
   FUNCTION fshr(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN SIGNED(fshr(UNSIGNED(arg1), arg2, arg1(arg1'LEFT), olen)); END;
   FUNCTION fshl(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN SIGNED(fshl(UNSIGNED(arg1), arg2, arg1(arg1'LEFT), olen)); END;
   FUNCTION fshr(arg1: SIGNED  ; arg2: SIGNED  ; olen: POSITIVE) RETURN SIGNED   IS
     BEGIN RETURN SIGNED(fshr(UNSIGNED(arg1), arg2, arg1(arg1'LEFT), olen)); END;


   FUNCTION frot(arg1: STD_LOGIC_VECTOR; arg2: STD_LOGIC_VECTOR; signd2: BOOLEAN; sdir: INTEGER range -1 TO 1) RETURN STD_LOGIC_VECTOR IS
     CONSTANT len: INTEGER := arg1'LENGTH;
     VARIABLE result: STD_LOGIC_VECTOR(len-1 DOWNTO 0);
     VARIABLE temp: STD_LOGIC_VECTOR(len-1 DOWNTO 0);
     SUBTYPE sw_range IS NATURAL range 0 TO len-1;
     VARIABLE sw: sw_range;
     VARIABLE temp_idx : INTEGER; --2.1.6.3
   BEGIN
     result := (others=>'0');
     result := arg1;
     sw := sdir MOD len;
     FOR i IN arg2'reverse_range LOOP
       EXIT WHEN sw = 0;
       IF signd2 AND i = arg2'LEFT THEN 
         sw := sub_int(len,sw); 
       END IF;
       -- temp := result(len-sw-1 DOWNTO 0) & result(len-1 DOWNTO len-sw)
       FOR i2 IN len-1 DOWNTO 0 LOOP
         --was: temp((i2+sw) MOD len) := result(i2);
         temp_idx := add_nat(i2,sw) MOD len;
         temp(temp_idx) := result(i2);
       END LOOP;
       result := mux_v(STD_LOGIC_VECTOR(concat_vect(result,temp)), arg2(i));
       sw := mod_natural(mult_natural(sw,2), len);
     END LOOP;
     RETURN result;
   END frot;

   FUNCTION frol(arg1: STD_LOGIC_VECTOR; arg2: UNSIGNED) RETURN STD_LOGIC_VECTOR IS
     BEGIN RETURN frot(arg1, STD_LOGIC_VECTOR(arg2), FALSE, 1); END;
   FUNCTION fror(arg1: STD_LOGIC_VECTOR; arg2: UNSIGNED) RETURN STD_LOGIC_VECTOR IS
     BEGIN RETURN frot(arg1, STD_LOGIC_VECTOR(arg2), FALSE, -1); END;
   FUNCTION frol(arg1: STD_LOGIC_VECTOR; arg2: SIGNED  ) RETURN STD_LOGIC_VECTOR IS
     BEGIN RETURN frot(arg1, STD_LOGIC_VECTOR(arg2), TRUE, 1); END;
   FUNCTION fror(arg1: STD_LOGIC_VECTOR; arg2: SIGNED  ) RETURN STD_LOGIC_VECTOR IS
     BEGIN RETURN frot(arg1, STD_LOGIC_VECTOR(arg2), TRUE, -1); END;

-----------------------------------------------------------------
-- indexing functions: LSB always has index 0
-----------------------------------------------------------------

   FUNCTION readindex(vec: STD_LOGIC_VECTOR; index: INTEGER                 ) RETURN STD_LOGIC IS
     CONSTANT len : INTEGER := vec'LENGTH;
     ALIAS    vec0: STD_LOGIC_VECTOR(len-1 DOWNTO 0) IS vec;
   BEGIN
     IF index >= len OR index < 0 THEN
       RETURN 'X';
     END IF;
     RETURN vec0(index);
   END;

   FUNCTION readslice(vec: STD_LOGIC_VECTOR; index: INTEGER; width: POSITIVE) RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := vec'LENGTH;
     CONSTANT indexPwidthM1 : INTEGER := index+width-1; --2.1.6.3
     ALIAS    vec0: STD_LOGIC_VECTOR(len-1 DOWNTO 0) IS vec;
     CONSTANT xxx : STD_LOGIC_VECTOR(width-1 DOWNTO 0) := (others => 'X');
   BEGIN
     IF index+width > len OR index < 0 THEN
       RETURN xxx;
     END IF;
     RETURN vec0(indexPwidthM1 DOWNTO index);
   END;

   FUNCTION writeindex(vec: STD_LOGIC_VECTOR; dinput: STD_LOGIC       ; index: INTEGER) RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := vec'LENGTH;
     VARIABLE vec0: STD_LOGIC_VECTOR(len-1 DOWNTO 0);
     CONSTANT xxx : STD_LOGIC_VECTOR(len-1 DOWNTO 0) := (others => 'X');
   BEGIN
     vec0 := vec;
     IF index >= len OR index < 0 THEN
       RETURN xxx;
     END IF;
     vec0(index) := dinput;
     RETURN vec0;
   END;

   FUNCTION n_bits(p: NATURAL) RETURN POSITIVE IS
     VARIABLE n_b : POSITIVE;
     VARIABLE p_v : NATURAL;
   BEGIN
     p_v := p;
     FOR i IN 1 TO 32 LOOP
       p_v := div_natural(p_v,2);
       n_b := i;
       EXIT WHEN (p_v = 0);
     END LOOP;
     RETURN n_b;
   END;


--   FUNCTION writeslice(vec: STD_LOGIC_VECTOR; dinput: STD_LOGIC_VECTOR; index: INTEGER) RETURN STD_LOGIC_VECTOR IS
--
--     CONSTANT vlen: INTEGER := vec'LENGTH;
--     CONSTANT ilen: INTEGER := dinput'LENGTH;
--     CONSTANT max_shift: INTEGER := vlen-ilen;
--     CONSTANT ones: UNSIGNED(ilen-1 DOWNTO 0) := (others => '1');
--     CONSTANT xxx : STD_LOGIC_VECTOR(vlen-1 DOWNTO 0) := (others => 'X');
--     VARIABLE shift : UNSIGNED(n_bits(max_shift)-1 DOWNTO 0);
--     VARIABLE vec0: STD_LOGIC_VECTOR(vlen-1 DOWNTO 0);
--     VARIABLE inp: UNSIGNED(vlen-1 DOWNTO 0);
--     VARIABLE mask: UNSIGNED(vlen-1 DOWNTO 0);
--   BEGIN
--     inp := (others => '0');
--     mask := (others => '0');
--
--     IF index > max_shift OR index < 0 THEN
--       RETURN xxx;
--     END IF;
--
--     shift := CONV_UNSIGNED(index, shift'LENGTH);
--     inp(ilen-1 DOWNTO 0) := UNSIGNED(dinput);
--     mask(ilen-1 DOWNTO 0) := ones;
--     inp := fshl(inp, shift, vlen);
--     mask := fshl(mask, shift, vlen);
--     vec0 := (vec and (not STD_LOGIC_VECTOR(mask))) or STD_LOGIC_VECTOR(inp);
--     RETURN vec0;
--   END;

   FUNCTION writeslice(vec: STD_LOGIC_VECTOR; dinput: STD_LOGIC_VECTOR; enable: STD_LOGIC_VECTOR; byte_width: INTEGER;  index: INTEGER) RETURN STD_LOGIC_VECTOR IS

     type enable_matrix is array (0 to enable'LENGTH-1 ) of std_logic_vector(byte_width-1 downto 0);
     CONSTANT vlen: INTEGER := vec'LENGTH;
     CONSTANT ilen: INTEGER := dinput'LENGTH;
     CONSTANT max_shift: INTEGER := vlen-ilen;
     CONSTANT ones: UNSIGNED(ilen-1 DOWNTO 0) := (others => '1');
     CONSTANT xxx : STD_LOGIC_VECTOR(vlen-1 DOWNTO 0) := (others => 'X');
     VARIABLE shift : UNSIGNED(n_bits(max_shift)-1 DOWNTO 0);
     VARIABLE vec0: STD_LOGIC_VECTOR(vlen-1 DOWNTO 0);
     VARIABLE inp: UNSIGNED(vlen-1 DOWNTO 0);
     VARIABLE mask: UNSIGNED(vlen-1 DOWNTO 0);
     VARIABLE mask2: UNSIGNED(vlen-1 DOWNTO 0);
     VARIABLE enables: enable_matrix;
     VARIABLE cat_enables: STD_LOGIC_VECTOR(ilen-1 DOWNTO 0 );
     VARIABLE lsbi : INTEGER;
     VARIABLE msbi : INTEGER;

   BEGIN
     cat_enables := (others => '0');
     lsbi := 0;
     msbi := byte_width-1;
     inp := (others => '0');
     mask := (others => '0');

     IF index > max_shift OR index < 0 THEN
       RETURN xxx;
     END IF;

     --initialize enables
     for i in 0 TO (enable'LENGTH-1) loop
       enables(i)  := (others => enable(i));
       cat_enables(msbi downto lsbi) := enables(i) ;
       lsbi := msbi+1;
       msbi := msbi+byte_width;
     end loop;


     shift := CONV_UNSIGNED(index, shift'LENGTH);
     inp(ilen-1 DOWNTO 0) := UNSIGNED(dinput);
     mask(ilen-1 DOWNTO 0) := UNSIGNED((STD_LOGIC_VECTOR(ones) AND cat_enables));
     inp := fshl(inp, shift, vlen);
     mask := fshl(mask, shift, vlen);
     vec0 := (vec and (not STD_LOGIC_VECTOR(mask))) or STD_LOGIC_VECTOR(inp);
     RETURN vec0;
   END;


   FUNCTION ceil_log2(size : NATURAL) return NATURAL is
     VARIABLE cnt : NATURAL;
     VARIABLE res : NATURAL;
   begin
     cnt := 1;
     res := 0;
     while (cnt < size) loop
       res := res + 1;
       cnt := 2 * cnt;
     end loop;
     return res;
   END;
   
   FUNCTION bits(size : NATURAL) return NATURAL is
   begin
     return ceil_log2(size);
   END;

   PROCEDURE csa(a, b, c: IN INTEGER; s, cout: OUT STD_LOGIC_VECTOR) IS
   BEGIN
     s    := conv_std_logic_vector(a, s'LENGTH) xor conv_std_logic_vector(b, s'LENGTH) xor conv_std_logic_vector(c, s'LENGTH);
     cout := ( (conv_std_logic_vector(a, cout'LENGTH) and conv_std_logic_vector(b, cout'LENGTH)) or (conv_std_logic_vector(a, cout'LENGTH) and conv_std_logic_vector(c, cout'LENGTH)) or (conv_std_logic_vector(b, cout'LENGTH) and conv_std_logic_vector(c, cout'LENGTH)) );
   END PROCEDURE csa;

   PROCEDURE csha(a, b: IN INTEGER; s, cout: OUT STD_LOGIC_VECTOR) IS
   BEGIN
     s    := conv_std_logic_vector(a, s'LENGTH) xor conv_std_logic_vector(b, s'LENGTH);
     cout := (conv_std_logic_vector(a, cout'LENGTH) and conv_std_logic_vector(b, cout'LENGTH));
   END PROCEDURE csha;

END funcs;

--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/mgc_shift_comps_v5.vhd 
LIBRARY ieee;

USE ieee.std_logic_1164.all;

PACKAGE mgc_shift_comps_v5 IS

COMPONENT mgc_shift_l_v5
  GENERIC (
    width_a: NATURAL;
    signd_a: NATURAL;
    width_s: NATURAL;
    width_z: NATURAL
  );
  PORT (
    a : in  std_logic_vector(width_a-1 DOWNTO 0);
    s : in  std_logic_vector(width_s-1 DOWNTO 0);
    z : out std_logic_vector(width_z-1 DOWNTO 0)
  );
END COMPONENT;

COMPONENT mgc_shift_r_v5
  GENERIC (
    width_a: NATURAL;
    signd_a: NATURAL;
    width_s: NATURAL;
    width_z: NATURAL
  );
  PORT (
    a : in  std_logic_vector(width_a-1 DOWNTO 0);
    s : in  std_logic_vector(width_s-1 DOWNTO 0);
    z : out std_logic_vector(width_z-1 DOWNTO 0)
  );
END COMPONENT;

COMPONENT mgc_shift_bl_v5
  GENERIC (
    width_a: NATURAL;
    signd_a: NATURAL;
    width_s: NATURAL;
    width_z: NATURAL
  );
  PORT (
    a : in  std_logic_vector(width_a-1 DOWNTO 0);
    s : in  std_logic_vector(width_s-1 DOWNTO 0);
    z : out std_logic_vector(width_z-1 DOWNTO 0)
  );
END COMPONENT;

COMPONENT mgc_shift_br_v5
  GENERIC (
    width_a: NATURAL;
    signd_a: NATURAL;
    width_s: NATURAL;
    width_z: NATURAL
  );
  PORT (
    a : in  std_logic_vector(width_a-1 DOWNTO 0);
    s : in  std_logic_vector(width_s-1 DOWNTO 0);
    z : out std_logic_vector(width_z-1 DOWNTO 0)
  );
END COMPONENT;

END mgc_shift_comps_v5;

--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/mgc_shift_r_beh_v5.vhd 
LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

ENTITY mgc_shift_r_v5 IS
  GENERIC (
    width_a: NATURAL;
    signd_a: NATURAL;
    width_s: NATURAL;
    width_z: NATURAL
  );
  PORT (
    a : in  std_logic_vector(width_a-1 DOWNTO 0);
    s : in  std_logic_vector(width_s-1 DOWNTO 0);
    z : out std_logic_vector(width_z-1 DOWNTO 0)
  );
END mgc_shift_r_v5;

LIBRARY ieee;

USE ieee.std_logic_arith.all;

ARCHITECTURE beh OF mgc_shift_r_v5 IS

  FUNCTION maximum (arg1,arg2: INTEGER) RETURN INTEGER IS
  BEGIN
    IF(arg1 > arg2) THEN
      RETURN(arg1) ;
    ELSE
      RETURN(arg2) ;
    END IF;
  END;

  FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN UNSIGNED IS
    CONSTANT ilen: INTEGER := arg1'LENGTH;
    CONSTANT olenM1: INTEGER := olen-1; -- 2.1.6.3
    CONSTANT ilenub: INTEGER := arg1'LENGTH-1;
    CONSTANT len: INTEGER := maximum(ilen, olen);
    VARIABLE result: UNSIGNED(len-1 DOWNTO 0);
  BEGIN
    result := (others => sbit);
    result(ilenub DOWNTO 0) := arg1;
    result := shr(result, arg2);
    RETURN result(olenM1 DOWNTO 0);
  END;

  FUNCTION fshr_stdar(arg1: SIGNED; arg2: UNSIGNED; sbit: STD_LOGIC; olen: POSITIVE) RETURN SIGNED IS
    CONSTANT ilen: INTEGER := arg1'LENGTH;
    CONSTANT olenM1: INTEGER := olen-1; -- 2.1.6.3
    CONSTANT ilenub: INTEGER := arg1'LENGTH-1;
    CONSTANT len: INTEGER := maximum(ilen, olen);
    VARIABLE result: SIGNED(len-1 DOWNTO 0);
  BEGIN
    result := (others => sbit);
    result(ilenub DOWNTO 0) := arg1;
    result := shr(result, arg2);
    RETURN result(olenM1 DOWNTO 0);
  END;

  FUNCTION fshr_stdar(arg1: UNSIGNED; arg2: UNSIGNED; olen: POSITIVE)
  RETURN UNSIGNED IS
  BEGIN
    RETURN fshr_stdar(arg1, arg2, '0', olen);
  END;

  FUNCTION fshr_stdar(arg1: SIGNED  ; arg2: UNSIGNED; olen: POSITIVE)
  RETURN SIGNED IS
  BEGIN
    RETURN fshr_stdar(arg1, arg2, arg1(arg1'LEFT), olen);
  END;

BEGIN
UNSGNED:  IF signd_a = 0 GENERATE
    z <= std_logic_vector(fshr_stdar(unsigned(a), unsigned(s), width_z));
  END GENERATE;
SGNED:  IF signd_a /= 0 GENERATE
    z <= std_logic_vector(fshr_stdar(  signed(a), unsigned(s), width_z));
  END GENERATE;
END beh;

--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_in_v1.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
--------------------------------------------------------------------------------

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

PACKAGE ccs_in_pkg_v1 IS

COMPONENT ccs_in_v1
  GENERIC (
    rscid    : INTEGER;
    width    : INTEGER
  );
  PORT (
    idat   : OUT std_logic_vector(width-1 DOWNTO 0);
    dat    : IN  std_logic_vector(width-1 DOWNTO 0)
  );
END COMPONENT;

END ccs_in_pkg_v1;

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all; -- Prevent STARC 2.1.1.2 violation

ENTITY ccs_in_v1 IS
  GENERIC (
    rscid : INTEGER;
    width : INTEGER
  );
  PORT (
    idat  : OUT std_logic_vector(width-1 DOWNTO 0);
    dat   : IN  std_logic_vector(width-1 DOWNTO 0)
  );
END ccs_in_v1;

ARCHITECTURE beh OF ccs_in_v1 IS
BEGIN

  idat <= dat;

END beh;


--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ram_sync_dualRW_be_generic.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2015 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
--------------------------------------------------------------------------------

LIBRARY ieee;

use IEEE.std_logic_1164.all ;
use IEEE.std_logic_arith.all ;

package ram_sync_dualRW_be_pkg is

  component ram_sync_dualRW_be 
   generic (ram_id           : integer;
            words            : integer;
            width            : integer;
            addr_width       : integer;
            a_reset_active   : integer;
            s_reset_active   : integer;
            enable_active    : integer;
            re_active        : integer;
            we_active        : integer;
            num_byte_enables : integer;
            clock_edge       : integer;
            no_of_RAM_dualRW_readwrite_port  : integer
           );
     port (
           data_in    : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0) ;
           addr       : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * addr_width) - 1 downto 0) ;
           re         : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           we         : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           data_out   : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0);
           clk        : in  std_logic;
           a_rst      : in std_logic;
           s_rst      : in std_logic;
           en         : in std_logic
          );
  end component;
  component ram_sync_dualRW_be_port 
   generic (ram_id           : integer;
            words            : integer;
            width            : integer;
            addr_width       : integer;
            a_reset_active   : integer;
            s_reset_active   : integer;
            enable_active    : integer;
            re_active        : integer;
            we_active        : integer;
            num_byte_enables : integer;
            clock_edge       : integer;
            no_of_RAM_dualRW_readwrite_port  : integer
           );
     port (
           data_in_d  : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0) ;
           addr_d     : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * addr_width) - 1 downto 0) ;
           re_d       : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           we_d       : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           data_out_d : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0);
           data_in    : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0) ;
           addr       : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * addr_width) - 1 downto 0) ;
           re         : out STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           we         : out STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           data_out   : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0);
           clk        : in  std_logic;
           a_rst      : in std_logic;
           s_rst      : in std_logic;
           en         : in std_logic
          );
  end component;
end ram_sync_dualRW_be_pkg;

LIBRARY ieee;

USE IEEE.std_logic_1164.all ;
USE IEEE.std_logic_arith.all ;
USE IEEE.std_logic_unsigned.all ;

USE work.ram_sync_dualRW_be_pkg.all;

  entity ram_sync_dualRW_be is
   generic (ram_id           : integer := 1;
            words            : integer := 512;
            width            : integer := 8;
            addr_width       : integer := 6;
            a_reset_active   : integer := 1;
            s_reset_active   : integer := 1;
            enable_active    : integer := 1;
            re_active        : integer := 1;
            we_active        : integer := 1;
            num_byte_enables : integer := 1;
            clock_edge       : integer := 1;
            no_of_RAM_dualRW_readwrite_port  : integer := 2
     );
     port (
           data_in    : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0) ;
           addr       : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * addr_width) - 1 downto 0) ;
           re         : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           we         : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           data_out   : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0);
           clk        : in  std_logic;
           a_rst      : in  std_logic;
           s_rst      : in  std_logic;
           en         : in std_logic
          );
  end ram_sync_dualRW_be ;
  
  architecture sim of ram_sync_dualRW_be is
    type mem_type is array ((words)-1 downto 0) of
                        STD_LOGIC_VECTOR(width - 1 downto 0) ;
    signal mem : mem_type ;
    signal data_ina  : STD_LOGIC_VECTOR(width-1 downto 0);
    signal data_inb  : STD_LOGIC_VECTOR(width-1 downto 0);
    signal rea       : STD_LOGIC_VECTOR(num_byte_enables - 1 downto 0);
    signal reb       : STD_LOGIC_VECTOR(num_byte_enables - 1 downto 0);
    signal wea       : STD_LOGIC_VECTOR(num_byte_enables - 1 downto 0);
    signal web       : STD_LOGIC_VECTOR(num_byte_enables - 1 downto 0);
    signal addra     : STD_LOGIC_VECTOR(addr_width - 1 downto 0);
    signal addrb     : STD_LOGIC_VECTOR(addr_width - 1 downto 0);
    signal data_outa : STD_LOGIC_VECTOR(width-1 downto 0);
    signal data_outb : STD_LOGIC_VECTOR(width-1 downto 0);

    constant byte_width : integer := width / num_byte_enables;

    -- ASSUMPTION: no_of_RAM_dualRW_be_readwrite_port has a fixed value of 2

  begin
    --synopsys translate_off
    I0 : process (clk)
      begin
        if ( clk'event and conv_integer(clk) = clock_edge ) then
          if ( conv_integer(en) = enable_active ) then
            for i in 0 to num_byte_enables - 1 loop
              if conv_integer(rea(i)) = re_active then 
                  data_outa((i+1)*byte_width-1 downto i*byte_width) <= mem(conv_integer(addra))((i+1)*byte_width -1 downto i*byte_width);
              else
                  data_outa((i+1)*byte_width-1 downto i*byte_width) <= (OTHERS => 'X');
              end if;
              if conv_integer(reb(i)) = re_active then 
                  data_outb((i+1)*byte_width-1 downto i*byte_width) <= mem(conv_integer(addrb))((i+1)*byte_width -1 downto i*byte_width);
              else
                  data_outb((i+1)*byte_width-1 downto i*byte_width) <= (OTHERS => 'X');
              end if;
              if conv_integer(wea(i)) = we_active then
                mem(conv_integer(addra))(i*byte_width+byte_width-1 downto i*byte_width) <= data_ina(i*byte_width+byte_width-1 downto i*byte_width);
              end if;
              if conv_integer(web(i)) = we_active then
                mem(conv_integer(addrb))(i*byte_width+byte_width-1 downto i*byte_width) <= data_inb(i*byte_width+byte_width-1 downto i*byte_width);
              end if;
            end loop;
          end if;
        end if;
      end process;

      data_out <= data_outa & data_outb;

      addra <= addr((2 * addr_width) - 1 downto addr_width);
      addrb <= addr(addr_width - 1 downto 0);
      data_ina <= data_in((2*width) - 1 downto width);
      data_inb <= data_in(width - 1 downto 0);
      rea <= re(2*num_byte_enables-1 downto 1*num_byte_enables);
      reb <= re(1*num_byte_enables-1 downto 0*num_byte_enables);
      wea <= we(2*num_byte_enables-1 downto 1*num_byte_enables);
      web <= we(1*num_byte_enables-1 downto 0*num_byte_enables);
    --synopsys translate_on
  end sim ;

LIBRARY ieee;

USE IEEE.std_logic_1164.all ;
USE IEEE.std_logic_arith.all ;
USE IEEE.std_logic_unsigned.all ;

USE work.ram_sync_dualRW_be_pkg.all;

  entity ram_sync_dualRW_be_port is
   generic (ram_id           : integer := 1;
            words            : integer := 512;
            width            : integer := 8;
            addr_width       : integer := 6;
            a_reset_active   : integer := 1;
            s_reset_active   : integer := 1;
            enable_active    : integer := 1;
            re_active        : integer := 1;
            we_active        : integer := 1;
            num_byte_enables : integer := 1;
            clock_edge       : integer := 1;
            no_of_RAM_dualRW_readwrite_port  : integer := 2
     );
     port (
           data_in_d  : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0) ;
           addr_d     : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * addr_width) - 1 downto 0) ;
           re_d       : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           we_d       : in  STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           data_out_d : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0);
           data_in    : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0) ;
           addr       : out STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * addr_width) - 1 downto 0) ;
           re         : out STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           we         : out STD_LOGIC_VECTOR(num_byte_enables*no_of_RAM_dualRW_readwrite_port - 1 downto 0);
           data_out   : in  STD_LOGIC_VECTOR((no_of_RAM_dualRW_readwrite_port * width) - 1 downto 0);
           clk        : in  std_logic;
           a_rst      : in  std_logic;
           s_rst      : in  std_logic;
           en         : in std_logic
          );
  end ram_sync_dualRW_be_port ;
  
  architecture sim of ram_sync_dualRW_be_port is
    begin
    data_in     <= data_in_d;
    addr        <= addr_d;
    re          <= re_d;
    we          <= we_d;
    data_out_d  <= data_out;
  end sim;
  

--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_genreg_v1.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
--------------------------------------------------------------------------------

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

PACKAGE ccs_genreg_pkg_v1 IS

  COMPONENT ccs_genreg_v1
    GENERIC (
      width    : INTEGER;
      ph_clk   : INTEGER RANGE 0 TO 1;
      ph_en    : INTEGER RANGE 0 TO 1;
      ph_arst  : INTEGER RANGE 0 TO 1;
      ph_srst  : INTEGER RANGE 0 TO 1;
      has_en   : INTEGER RANGE 0 TO 1
    );
    PORT (
      clk     : IN  std_logic;
      en      : IN  std_logic;
      arst    : IN  std_logic;
      srst    : IN  std_logic;
      d       : IN  std_logic_vector(width-1 DOWNTO 0);
      z       : OUT std_logic_vector(width-1 DOWNTO 0)
    );
  END COMPONENT;
END ccs_genreg_pkg_v1;

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY ccs_genreg_v1 IS
  GENERIC (
    width    : INTEGER;
    ph_clk   : INTEGER RANGE 0 TO 1;
    ph_en    : INTEGER RANGE 0 TO 1;
    ph_arst  : INTEGER RANGE 0 TO 1;
    ph_srst  : INTEGER RANGE 0 TO 1;
    has_en   : INTEGER RANGE 0 TO 1
  );
  PORT (
    clk     : IN  std_logic;
    en      : IN  std_logic;
    arst    : IN  std_logic;
    srst    : IN  std_logic;
    d       : IN  std_logic_vector(width-1 DOWNTO 0);
    z       : OUT std_logic_vector(width-1 DOWNTO 0)
  );
END ccs_genreg_v1;

ARCHITECTURE beh OF ccs_genreg_v1 IS
BEGIN
    GEN_REG_POS_CLK: IF ph_clk = 1 GENERATE
      GEN_CLK1_EN: IF has_en = 1 GENERATE
        PROCESS (clk, arst)
        BEGIN
          IF (conv_integer(arst) = ph_arst) THEN
            z <= (others => '0');
          ELSIF (clk'EVENT AND clk = '1') THEN
            IF (conv_integer(srst) = ph_srst) THEN
              z <= (others => '0');
            ELSIF (conv_integer(en) = ph_en) THEN
              z <= d;
            END IF;
          END IF;
        END PROCESS;
      END GENERATE GEN_CLK1_EN;

      GEN_CLK1_NO_EN: IF has_en = 0 GENERATE
        PROCESS (clk, arst)
        BEGIN
          IF (conv_integer(arst) = ph_arst) THEN
            z <= (others => '0');
          ELSIF (clk'EVENT AND clk = '1') THEN
            IF (conv_integer(srst) = ph_srst) THEN
              z <= (others => '0');
            ELSE
              z <= d;
            END IF;
          END IF;
        END PROCESS;
      END GENERATE GEN_CLK1_NO_EN;
    END GENERATE GEN_REG_POS_CLK;


    GEN_REG_NEG_CLK: IF ph_clk = 0 GENERATE
      GEN_CLK0_EN: IF has_en = 1 GENERATE
        PROCESS (clk, arst)
        BEGIN
          IF (conv_integer(arst) = ph_arst) THEN
            z <= (others => '0');
          ELSIF (clk'EVENT AND clk = '0') THEN
            IF (conv_integer(srst) = ph_srst) THEN
              z <= (others => '0');
            ELSIF (conv_integer(en) = ph_en) THEN
              z <= d;
            END IF;
          END IF;
        END PROCESS;
      END GENERATE GEN_CLK0_EN;

      GEN_CLK0_NO_EN: IF has_en = 0 GENERATE
        PROCESS (clk, arst)
        BEGIN
          IF (conv_integer(arst) = ph_arst) THEN
            z <= (others => '0');
          ELSIF (clk'EVENT AND clk = '0') THEN
            IF (conv_integer(srst) = ph_srst) THEN
              z <= (others => '0');
            ELSE
              z <= d;
            END IF;
          END IF;
        END PROCESS;
      END GENERATE GEN_CLK0_NO_EN;
    END GENERATE GEN_REG_NEG_CLK;
END beh;


--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_fifo_wait_core_v5.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--
-- Change History:
--    2019-01-24 - Verify and fix bug in rdy signal behavior under reset.
--                 Provide parameter defaults
----------------------------------------------------------------------------------

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

PACKAGE ccs_fifo_wait_core_pkg_v5 IS

  COMPONENT ccs_fifo_wait_core_v5
    GENERIC (
      rscid    : INTEGER := 0;
      width    : INTEGER := 8;
      sz_width : INTEGER := 8;
      fifo_sz  : INTEGER := 8;
      ph_clk   : INTEGER RANGE 0 TO 1 := 1;
      ph_en    : INTEGER RANGE 0 TO 1 := 1;
      ph_arst  : INTEGER RANGE 0 TO 1 := 1;
      ph_srst  : INTEGER RANGE 0 TO 1 := 1;
      ph_log2  : INTEGER := 3
    );
    PORT (
      clk      : IN  std_logic;
      en       : IN  std_logic;
      arst     : IN  std_logic;
      srst     : IN  std_logic;
      din_vld  : IN  std_logic;
      din_rdy  : OUT std_logic;
      din      : IN  std_logic_vector(width-1 DOWNTO 0);
      dout_vld : OUT std_logic;
      dout_rdy : IN  std_logic;
      dout     : OUT std_logic_vector(width-1 DOWNTO 0);
      sd       : OUT std_logic_vector(sz_width-1 DOWNTO 0);
      is_idle  : OUT std_logic
    );
  END COMPONENT;
END ccs_fifo_wait_core_pkg_v5;

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

USE work.ccs_genreg_pkg_v1.all;

ENTITY ccs_fifo_wait_core_v5 IS
  GENERIC (
    rscid    : INTEGER := 0;
    width    : INTEGER := 8;
    sz_width : INTEGER := 8;
    fifo_sz  : INTEGER := 8;
    ph_clk   : INTEGER RANGE 0 TO 1 := 1;
    ph_en    : INTEGER RANGE 0 TO 1 := 1;
    ph_arst  : INTEGER RANGE 0 TO 1 := 1;
    ph_srst  : INTEGER RANGE 0 TO 1 := 1;
    ph_log2  : INTEGER := 3
  );
  PORT (
    clk      : IN  std_logic;
    en       : IN  std_logic;
    arst     : IN  std_logic;
    srst     : IN  std_logic;
    din_vld  : IN  std_logic;
    din_rdy  : OUT std_logic;
    din      : IN  std_logic_vector(width-1 DOWNTO 0);
    dout_vld : OUT std_logic;
    dout_rdy : IN  std_logic;
    dout     : OUT std_logic_vector(width-1 DOWNTO 0);
    sd       : OUT std_logic_vector(sz_width-1 DOWNTO 0);
    is_idle  : OUT std_logic
  );
END ccs_fifo_wait_core_v5;

ARCHITECTURE beh OF ccs_fifo_wait_core_v5 IS

  FUNCTION adjust(sz : INTEGER) RETURN INTEGER IS
    VARIABLE res : INTEGER RANGE 0 TO width*fifo_sz+1;
  BEGIN
    IF sz > 0 THEN
      res := sz;
    ELSE
      res := 1;
    END IF;
    RETURN res;
  END ;

  SIGNAL stat     : std_logic_vector(adjust(fifo_sz)-1 DOWNTO 0);
  SIGNAL stat_pre : std_logic_vector(adjust(fifo_sz)-1 DOWNTO 0);
  SIGNAL hs_init  : std_logic_vector(0 DOWNTO 0);

  SIGNAL sbuf     : std_logic_vector(adjust(width*fifo_sz)-1 DOWNTO 0);
  SIGNAL buf_pre  : std_logic_vector(adjust(width*fifo_sz)-1 DOWNTO 0);

  SIGNAL en_l     : std_logic_vector(adjust(fifo_sz)-1 DOWNTO 0);
  SIGNAL en_l_s   : std_logic_vector(((adjust(fifo_sz)-1)/8) DOWNTO 0);

  SIGNAL din_rdy_drv  : std_logic;
  SIGNAL dout_vld_drv : std_logic;
  SIGNAL din_vld_int   : std_logic;
  SIGNAL active       : std_logic;

  SIGNAL count  : integer range 0 to fifo_sz+1 := 0;
  -- pragma translate_off
  SIGNAL peak   : integer range 0 to fifo_sz+1 := 0;
  -- pragma translate_on

BEGIN
  --din_rdy  <= din_rdy_drv;
  -- din_rdy_drv  <= dout_rdy OR (NOT stat(0) AND hs_init(0));
  din_rdy  <= '1' WHEN ((fifo_sz > 0) and (((stat(0) = '0') or (dout_rdy = '1')) and (hs_init(0) = '1'))) or
                       ((fifo_sz <= 0) and (dout_rdy = '1')) else '0';
  
  dout_vld <= dout_vld_drv;
  is_idle  <= (NOT ((din_vld and din_rdy_drv) or (dout_vld_drv and dout_rdy))) and hs_init(0);
  
  FIFO_REG: IF fifo_sz > 0 GENERATE
    din_vld_int  <= din_vld AND hs_init(0);
    din_rdy_drv  <= dout_rdy OR (NOT stat(0) AND hs_init(0));
    dout_vld_drv <= din_vld_int OR stat(fifo_sz-1);

    active       <= (din_vld_int AND din_rdy_drv) OR (dout_rdy AND dout_vld_drv);

    sd <= conv_std_logic_vector(count 
            - (conv_integer(dout_rdy AND stat(fifo_sz-1)))
            + conv_integer(din_vld_int)
            , sz_width);   -- 32 == sz_width;

    DO_PROC: PROCESS(sbuf, din, stat)
    BEGIN
      IF conv_integer(stat(fifo_sz-1)) = 1 THEN
        dout <= sbuf(width*fifo_sz-1 DOWNTO width*(fifo_sz-1));
      ELSE
        dout <= din; -- pass through
     END IF;
    END PROCESS;
  
    FIFOPROC: PROCESS(din_vld_int, dout_rdy, din, stat, sbuf, en, active)
      VARIABLE stat_ahead,
               stat_behind,
               stat_nxt,
               en_l_var : std_logic;
      VARIABLE buf_nxt  : std_logic_vector(width-1 DOWNTO 0);
      VARIABLE n_elem   : integer range 0 to fifo_sz+1;
      VARIABLE count_t  : integer range 0 to fifo_sz+1;
    BEGIN
      n_elem := 0;
      FOR i IN fifo_sz-1 DOWNTO 0 LOOP
        IF i /= 0         THEN stat_behind := stat(i-1); ELSE stat_behind := '0'; END IF;
        IF i /= fifo_sz-1 THEN stat_ahead  := stat(i+1); ELSE stat_ahead  := '1'; END IF;

        -- Determine if this buffer element will have data
        stat_nxt := stat_ahead AND                        -- valid element ahead of this one (or head)
                      (stat_behind                        -- valid element behind this one
                        OR (stat(i) AND NOT dout_rdy)     -- valid element, output not ready (in use, no tx)
                        OR (stat(i) AND din_vld_int)      -- valid element and input has data
                        OR (din_vld_int AND NOT dout_rdy) -- input has data and output not ready
                      );
        stat_pre(i) <= stat_nxt;

        IF conv_integer(dout_rdy AND stat_behind) = 1 THEN
          -- Pop n Shift 
          buf_nxt := sbuf(width*i-1 DOWNTO width*(i-1));
          en_l_var := '1';                             
        ELSIF conv_integer(din_vld_int AND stat_nxt AND    NOT(NOT dout_rdy AND stat(i))) =  1 THEN
          -- Push input on to buffer       ^will have data ^-- ??? not already in use, no tx ???
          buf_nxt := din;
          en_l_var := '1';
        ELSE
          buf_nxt := (others => 'X'); --din; -- Don't care input to disabled flop
          en_l_var := '0';
        END IF;

        buf_pre(width*(i+1)-1 DOWNTO width*i) <= buf_nxt;

        IF conv_integer(ph_en) = 1 THEN
          en_l(i) <= en AND en_l_var;
        ELSE
          en_l(i) <= en OR (NOT en_l_var);
        END IF;

        IF (stat_ahead = '1' AND stat(i) = '0') then
          -- Found tail, update number of elements for count
          n_elem := fifo_sz - 1 - i;
        END IF;
      END LOOP;

      -- Enable for stat registers (partitioned into banks of eight)
      -- Take care of the head first
      IF conv_integer(ph_en) = 1 THEN
        en_l_s((adjust(fifo_sz)-1)/8) <= en AND active;
      ELSE
        en_l_s((adjust(fifo_sz)-1)/8) <= en OR NOT active;
      END IF;
      -- Now every eight
      FOR i IN fifo_sz-1 DOWNTO 7 LOOP
        IF (i rem 8) = 0 THEN
          IF conv_integer(ph_en) = 1 THEN
            en_l_s((i/8)-1) <= en AND stat(i) AND active;
          ELSE
            en_l_s((i/8)-1) <= en OR (NOT stat(i)) OR NOT active;
          END IF;
        END IF;
      END LOOP;

      IF stat(fifo_sz-1) = '0' THEN
        count_t := 0;
      ELSIF stat(0) = '1' THEN
        count_t := fifo_sz;
      ELSE
        count_t := n_elem;
      END IF;
      count <= count_t;
      -- pragma translate_off
      IF ( peak < count_t ) THEN
        peak <= count_t;
      END IF;
      -- pragma translate_on
    END PROCESS;

    HS_INIT_REG : ccs_genreg_v1
      GENERIC MAP (
        width   => 1,
        ph_clk  => ph_clk,
        ph_en   => 1,
        ph_arst => ph_arst,
        ph_srst => ph_srst,
        has_en  => 0
      )
      PORT MAP (
        clk     => clk,
        en      => '1',
        arst    => arst,
        srst    => srst,
        d       => "1",
        z       => hs_init
      );

    GEN_REGS: FOR i IN fifo_sz-1 DOWNTO 0 GENERATE
      STATREG : ccs_genreg_v1
        GENERIC MAP (
          width   => 1,
          ph_clk  => ph_clk,
          ph_en   => ph_en,
          ph_arst => ph_arst,
          ph_srst => ph_srst,
          has_en  => 1
        )
        PORT MAP (
          clk     => clk,
          en      => en_l_s(i/8),
          arst    => arst,
          srst    => srst,
          d       => stat_pre(i DOWNTO i),
          z       => stat(i DOWNTO i)
        );

      BUFREG : ccs_genreg_v1
        GENERIC MAP (
          width   => width,
          ph_clk  => ph_clk,
          ph_en   => ph_en,
          ph_arst => ph_arst,
          ph_srst => ph_srst,
          has_en  => 1
        )
        PORT MAP (
          clk     => clk,
          en      => en_l(i),
          arst    => arst,
          srst    => srst,
          d       => buf_pre(width*(i+1)-1 DOWNTO width*i),
          z       => sbuf(width*(i+1)-1 DOWNTO width*i) 
        );
    END GENERATE GEN_REGS;
  END GENERATE FIFO_REG;

  FEED_THRU: IF fifo_sz = 0 GENERATE
    din_rdy_drv  <= dout_rdy;
    dout_vld_drv <= din_vld;
    dout         <= din;
    -- non-blocking is not II=1 when fifo_sz=0
    sd <= conv_std_logic_vector (conv_integer(din_vld AND NOT dout_rdy), sz_width);
  END GENERATE FEED_THRU;
END beh;



--------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_pipe_v5.vhd 
--------------------------------------------------------------------------------
-- Catapult Synthesis - Sample I/O Port Library
--
-- Copyright (c) 2003-2017 Mentor Graphics Corp.
--       All Rights Reserved
--
-- This document may be used and distributed without restriction provided that
-- this copyright statement is not removed from the file and that any derivative
-- work contains this copyright notice.
--
-- The design information contained in this file is intended to be an example
-- of the functionality which the end user may study in preparation for creating
-- their own custom interfaces. This design does not necessarily present a 
-- complete implementation of the named protocol or standard.
--------------------------------------------------------------------------------
--
--            ________________________________________________
-- WRITER    |                                                |          READER
--           |                    ccs_pipe                    |
--           |            ________________________            |
--        --<| din_rdy --<|  ------------------ <|---dout_rdy<|---
--           |            |         FIFO         |            |
--        ---|>din_vld ---|> ------------------  |>--dout_vld |>--
--        ---|>din -------|> ------------------  |> -----dout |>--
--           |            |______________________|            |
--           |________________________________________________|
--
--    din_rdy     - can be considered as a notFULL signal
--    dout_vld    - can be considered as a notEMPTY signal
--    write_stall - an internal debug signal formed from din_vld & !din_rdy
--    read_stall  - an internal debug signal formed from dout_rdy & !dout_vld
--    is_idle     - indicates the clock can be safely gated
--

LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

PACKAGE ccs_pipe_pkg_v5 IS
  COMPONENT ccs_pipe_v5
    GENERIC (
      rscid    : INTEGER := 0;
      width    : INTEGER := 8;
      sz_width : INTEGER := 8;
      fifo_sz  : INTEGER := 8;
      log2_sz  : INTEGER := 3;
      ph_clk   : INTEGER RANGE 0 TO 1 := 1;
      ph_en    : INTEGER RANGE 0 TO 1 := 1;
      ph_arst  : INTEGER RANGE 0 TO 1 := 1;
      ph_srst  : INTEGER RANGE 0 TO 1 := 1
    );
    PORT (
      -- clock
      clk      : IN  std_logic;
      en       : IN  std_logic;
      arst     : IN  std_logic;
      srst     : IN  std_logic;
      -- writer
      din_rdy  : OUT std_logic;
      din_vld  : IN  std_logic;
      din      : IN  std_logic_vector(width-1 DOWNTO 0);
      -- reader
      dout_rdy : IN  std_logic;
      dout_vld : OUT std_logic;
      dout     : OUT std_logic_vector(width-1 DOWNTO 0);
      -- size
      sz       : OUT std_logic_vector(sz_width-1 DOWNTO 0);
      sz_req   : IN  std_logic;
      is_idle  : OUT std_logic
    );
  END COMPONENT;
END ccs_pipe_pkg_v5;


LIBRARY ieee;

USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

USE work.ccs_fifo_wait_core_pkg_v5.all;

ENTITY ccs_pipe_v5 IS
  GENERIC (
    rscid    : INTEGER := 0;
    width    : INTEGER := 8;
    sz_width : INTEGER := 8;
    fifo_sz  : INTEGER := 8;
    log2_sz  : INTEGER := 3;
    ph_clk   : INTEGER RANGE 0 TO 1 := 1;
    ph_en    : INTEGER RANGE 0 TO 1 := 1;
    ph_arst  : INTEGER RANGE 0 TO 1 := 1;
    ph_srst  : INTEGER RANGE 0 TO 1 := 1
  );
  PORT (
    -- clock
    clk      : IN  std_logic;
    en       : IN  std_logic;
    arst     : IN  std_logic;
    srst     : IN  std_logic;
    -- writer
    din_rdy  : OUT std_logic;
    din_vld  : IN  std_logic;
    din      : IN  std_logic_vector(width-1 DOWNTO 0);
    -- reader
    dout_rdy : IN  std_logic;
    dout_vld : OUT std_logic;
    dout     : OUT std_logic_vector(width-1 DOWNTO 0);
    -- size
    sz       : OUT std_logic_vector(sz_width-1 DOWNTO 0);
    sz_req   : in  std_logic;
    is_idle  : OUT std_logic
  );
END ccs_pipe_v5;

ARCHITECTURE beh OF ccs_pipe_v5 IS

  SIGNAL din_rdy_drv : std_logic;
  SIGNAL dout_vld_drv : std_logic;

  -- Internal debug signals
  -- pragma translate_off
  SIGNAL write_stall : std_logic;
  SIGNAL read_stall : std_logic;
  -- pragma translate_on

BEGIN

  din_rdy  <= din_rdy_drv;
  dout_vld <= dout_vld_drv;

  -- pragma translate_off
  write_stall <= din_vld  AND NOT din_rdy_drv;
  read_stall  <= dout_rdy AND NOT dout_vld_drv;
  -- pragma translate_on

  FIFO: ccs_fifo_wait_core_v5
    generic map (
      rscid    => rscid,
      width    => width,
      sz_width => sz_width,
      fifo_sz  => fifo_sz,
      ph_clk   => ph_clk,
      ph_en    => ph_en,
      ph_arst  => ph_arst,
      ph_srst  => ph_srst,
      ph_log2  => log2_sz
    )
    port map (
      clk      => clk,
      en       => en,
      arst     => arst,
      srst     => srst,
      din_vld  => din_vld,
      din_rdy  => din_rdy_drv,
      din      => din,
      dout_vld => dout_vld_drv,
      dout_rdy => dout_rdy,
      dout     => dout,
      sd       => sz,
      is_idle  => is_idle
    );

END beh;


--------> ./rtl_hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60.vhdl 
-- ----------------------------------------------------------------------
--  HLS HDL:        VHDL Netlister
--  HLS Version:    10.5a/871028 Production Release
--  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
-- 
--  Generated by:   user2@edatools.ee.duth.gr
--  Generated date: Sun Jul  4 16:59:11 2021
-- ----------------------------------------------------------------------

-- 
LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


PACKAGE hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60_pkg IS 
  COMPONENT hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
  END COMPONENT;
END hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60_pkg;

PACKAGE BODY hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60_pkg IS
END hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60_pkg;

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


USE work.hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60_pkg.all;
LIBRARY std;

USE std.textio.all;
USE ieee.std_logic_textio.all;

ENTITY hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60 IS
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60;

ARCHITECTURE v1 OF hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60 IS
  -- Start of SIF_NL_VHDL::nhl_rom_body
  -- Constants for ROM dimensions
  CONSTANT n_width    : INTEGER := 32;
  CONSTANT n_size     : INTEGER := 70;
  CONSTANT n_addr_w   : INTEGER := 7;
  CONSTANT n_inreg    : INTEGER := 0;
  CONSTANT n_outreg   : INTEGER := 0;
  -- Define data types for ROM storage;
  SUBTYPE  word  IS std_logic_vector((n_width)-1 DOWNTO 0);
  TYPE     table IS ARRAY (0 to n_size-1) of word;

  SIGNAL mem : table := table'(
    word'("10000000000000000000000000000000"),
    word'("01001011100100000001010001110110"),
    word'("00100111111011001110000101101101"),
    word'("00010100010001000100011101010000"),
    word'("00001010001011000011010100001100"),
    word'("00000101000101110101111110000101"),
    word'("00000010100010111101100001111001"),
    word'("00000001010001011111000101010100"),
    word'("00000000101000101111100101001101"),
    word'("00000000010100010111110010111010"),
    word'("00000000001010001011111001100000"),
    word'("00000000000101000101111100110000"),
    word'("00000000000010100010111110011000"),
    word'("00000000000001010001011111001100"),
    word'("00000000000000101000101111100110"),
    word'("00000000000000010100010111110011"),
    word'("00000000000000001010001011111001"),
    word'("00000000000000000101000101111100"),
    word'("00000000000000000010100010111110"),
    word'("00000000000000000001010001011111"),
    word'("00000000000000000000101000101111"),
    word'("00000000000000000000010100010111"),
    word'("00000000000000000000001010001011"),
    word'("00000000000000000000000101000101"),
    word'("00000000000000000000000010100010"),
    word'("00000000000000000000000001010001"),
    word'("00000000000000000000000000101000"),
    word'("00000000000000000000000000010100"),
    word'("00000000000000000000000000001010"),
    word'("00000000000000000000000000000101"),
    word'("00000000000000000000000000000010"),
    word'("00000000000000000000000000000001"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000")
  );
BEGIN

  -- Reading ROM
  PROCESS(addr)
    VARIABLE idx_addr : INTEGER;
  BEGIN
    idx_addr := conv_integer(unsigned(addr(6 DOWNTO 0)));
    IF idx_addr >= 0 AND idx_addr < 70 THEN
      data_out <= mem(idx_addr);
    ELSE
      idx_addr := conv_integer(unsigned(addr(n_addr_w-2 DOWNTO 0)));
      data_out <= mem(idx_addr);
    END IF;
  END PROCESS;

END v1;



--------> ./rtl_hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60.vhdl 
-- ----------------------------------------------------------------------
--  HLS HDL:        VHDL Netlister
--  HLS Version:    10.5a/871028 Production Release
--  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
-- 
--  Generated by:   user2@edatools.ee.duth.gr
--  Generated date: Sun Jul  4 16:59:11 2021
-- ----------------------------------------------------------------------

-- 
LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


PACKAGE hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60_pkg IS 
  COMPONENT hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
  END COMPONENT;
END hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60_pkg;

PACKAGE BODY hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60_pkg IS
END hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60_pkg;

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


USE work.hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60_pkg.all;
LIBRARY std;

USE std.textio.all;
USE ieee.std_logic_textio.all;

ENTITY hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60 IS
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60;

ARCHITECTURE v1 OF hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60 IS
  -- Start of SIF_NL_VHDL::nhl_rom_body
  -- Constants for ROM dimensions
  CONSTANT n_width    : INTEGER := 32;
  CONSTANT n_size     : INTEGER := 70;
  CONSTANT n_addr_w   : INTEGER := 7;
  CONSTANT n_inreg    : INTEGER := 0;
  CONSTANT n_outreg   : INTEGER := 0;
  -- Define data types for ROM storage;
  SUBTYPE  word  IS std_logic_vector((n_width)-1 DOWNTO 0);
  TYPE     table IS ARRAY (0 to n_size-1) of word;

  SIGNAL mem : table := table'(
    word'("10000000000000000000000000000000"),
    word'("01001011100100000001010001110110"),
    word'("00100111111011001110000101101101"),
    word'("00010100010001000100011101010000"),
    word'("00001010001011000011010100001100"),
    word'("00000101000101110101111110000101"),
    word'("00000010100010111101100001111001"),
    word'("00000001010001011111000101010100"),
    word'("00000000101000101111100101001101"),
    word'("00000000010100010111110010111010"),
    word'("00000000001010001011111001100000"),
    word'("00000000000101000101111100110000"),
    word'("00000000000010100010111110011000"),
    word'("00000000000001010001011111001100"),
    word'("00000000000000101000101111100110"),
    word'("00000000000000010100010111110011"),
    word'("00000000000000001010001011111001"),
    word'("00000000000000000101000101111100"),
    word'("00000000000000000010100010111110"),
    word'("00000000000000000001010001011111"),
    word'("00000000000000000000101000101111"),
    word'("00000000000000000000010100010111"),
    word'("00000000000000000000001010001011"),
    word'("00000000000000000000000101000101"),
    word'("00000000000000000000000010100010"),
    word'("00000000000000000000000001010001"),
    word'("00000000000000000000000000101000"),
    word'("00000000000000000000000000010100"),
    word'("00000000000000000000000000001010"),
    word'("00000000000000000000000000000101"),
    word'("00000000000000000000000000000010"),
    word'("00000000000000000000000000000001"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000"),
    word'("00000000000000000000000000000000")
  );
BEGIN

  -- Reading ROM
  PROCESS(addr)
    VARIABLE idx_addr : INTEGER;
  BEGIN
    idx_addr := conv_integer(unsigned(addr(6 DOWNTO 0)));
    IF idx_addr >= 0 AND idx_addr < 70 THEN
      data_out <= mem(idx_addr);
    ELSE
      idx_addr := conv_integer(unsigned(addr(n_addr_w-2 DOWNTO 0)));
      data_out <= mem(idx_addr);
    END IF;
  END PROCESS;

END v1;



--------> ./rtl.vhdl 
-- ----------------------------------------------------------------------
--  HLS HDL:        VHDL Netlister
--  HLS Version:    10.5a/871028 Production Release
--  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
-- 
--  Generated by:   user2@edatools.ee.duth.gr
--  Generated date: Sun Jul  4 16:59:11 2021
-- ----------------------------------------------------------------------

-- 
-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_core_fsm
--  FSM Module
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_core_fsm IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    fsm_output : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
    T_LINE_C_4_tr0 : IN STD_LOGIC;
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
        : IN STD_LOGIC;
    T_LINE_C_10_tr0 : IN STD_LOGIC;
    T_LINE_C_10_tr1 : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
        : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
        : IN STD_LOGIC;
    T_LINE_C_14_tr0 : IN STD_LOGIC;
    T_LINE_C_14_tr1 : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
        : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
        : IN STD_LOGIC;
    T_LINE_C_18_tr0 : IN STD_LOGIC;
    R_LINE_C_0_tr0 : IN STD_LOGIC
  );
END getMaxLine_core_core_fsm;

ARCHITECTURE v1 OF getMaxLine_core_core_fsm IS
  -- Default Constants

  -- FSM State Type Declaration for getMaxLine_core_core_fsm_1
  TYPE getMaxLine_core_core_fsm_1_ST IS (main_C_0, T_LINE_C_0, T_LINE_C_1, T_LINE_C_2,
      T_LINE_C_3, T_LINE_C_4, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5,
      T_LINE_C_5, T_LINE_C_6, T_LINE_C_7, T_LINE_C_8, T_LINE_C_9, T_LINE_C_10, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0,
      T_LINE_C_11, T_LINE_C_12, T_LINE_C_13, T_LINE_C_14, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0,
      T_LINE_C_15, T_LINE_C_16, T_LINE_C_17, T_LINE_C_18, R_LINE_C_0, main_C_1);

  SIGNAL state_var : getMaxLine_core_core_fsm_1_ST;
  SIGNAL state_var_NS : getMaxLine_core_core_fsm_1_ST;

BEGIN
  getMaxLine_core_core_fsm_1 : PROCESS (T_LINE_C_4_tr0, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0,
      T_LINE_C_10_tr0, T_LINE_C_10_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_14_tr0, T_LINE_C_14_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_18_tr0, R_LINE_C_0_tr0, state_var)
  BEGIN
    CASE state_var IS
      WHEN T_LINE_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000000010");
        state_var_NS <= T_LINE_C_1;
      WHEN T_LINE_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000000100");
        state_var_NS <= T_LINE_C_2;
      WHEN T_LINE_C_2 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000001000");
        state_var_NS <= T_LINE_C_3;
      WHEN T_LINE_C_3 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000010000");
        state_var_NS <= T_LINE_C_4;
      WHEN T_LINE_C_4 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000100000");
        IF ( T_LINE_C_4_tr0 = '1' ) THEN
          state_var_NS <= T_LINE_C_5;
        ELSE
          state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        END IF;
      WHEN ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000001000000");
        state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1;
      WHEN ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000010000000");
        state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2;
      WHEN ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000100000000");
        state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3;
      WHEN ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000001000000000");
        state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4;
      WHEN ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000010000000000");
        state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5;
      WHEN ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000100000000000");
        IF ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_5;
        ELSE
          state_var_NS <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        END IF;
      WHEN T_LINE_C_5 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000001000000000000");
        state_var_NS <= T_LINE_C_6;
      WHEN T_LINE_C_6 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000010000000000000");
        state_var_NS <= T_LINE_C_7;
      WHEN T_LINE_C_7 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000100000000000000");
        state_var_NS <= T_LINE_C_8;
      WHEN T_LINE_C_8 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000001000000000000000");
        state_var_NS <= T_LINE_C_9;
      WHEN T_LINE_C_9 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000010000000000000000");
        state_var_NS <= T_LINE_C_10;
      WHEN T_LINE_C_10 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000100000000000000000");
        IF ( T_LINE_C_10_tr0 = '1' ) THEN
          state_var_NS <= T_LINE_C_11;
        ELSIF ( T_LINE_C_10_tr1 = '1' ) THEN
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000001000000000000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_11;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000010000000000000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_11;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        END IF;
      WHEN T_LINE_C_11 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000100000000000000000000");
        state_var_NS <= T_LINE_C_12;
      WHEN T_LINE_C_12 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000001000000000000000000000");
        state_var_NS <= T_LINE_C_13;
      WHEN T_LINE_C_13 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000010000000000000000000000");
        state_var_NS <= T_LINE_C_14;
      WHEN T_LINE_C_14 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000100000000000000000000000");
        IF ( T_LINE_C_14_tr0 = '1' ) THEN
          state_var_NS <= T_LINE_C_15;
        ELSIF ( T_LINE_C_14_tr1 = '1' ) THEN
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000001000000000000000000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_15;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000010000000000000000000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_15;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        END IF;
      WHEN T_LINE_C_15 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000100000000000000000000000000");
        state_var_NS <= T_LINE_C_16;
      WHEN T_LINE_C_16 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00001000000000000000000000000000");
        state_var_NS <= T_LINE_C_17;
      WHEN T_LINE_C_17 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00010000000000000000000000000000");
        state_var_NS <= T_LINE_C_18;
      WHEN T_LINE_C_18 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00100000000000000000000000000000");
        IF ( T_LINE_C_18_tr0 = '1' ) THEN
          state_var_NS <= R_LINE_C_0;
        ELSE
          state_var_NS <= T_LINE_C_0;
        END IF;
      WHEN R_LINE_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "01000000000000000000000000000000");
        IF ( R_LINE_C_0_tr0 = '1' ) THEN
          state_var_NS <= main_C_1;
        ELSE
          state_var_NS <= T_LINE_C_0;
        END IF;
      WHEN main_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "10000000000000000000000000000000");
        state_var_NS <= main_C_0;
      -- main_C_0
      WHEN OTHERS =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000000001");
        state_var_NS <= T_LINE_C_0;
    END CASE;
  END PROCESS getMaxLine_core_core_fsm_1;

  getMaxLine_core_core_fsm_1_REG : PROCESS (clk)
  BEGIN
    IF clk'event AND ( clk = '1' ) THEN
      IF ( rst = '1' ) THEN
        state_var <= main_C_0;
      ELSE
        IF ( core_wen = '1' ) THEN
          state_var <= state_var_NS;
        END IF;
      END IF;
    END IF;
  END PROCESS getMaxLine_core_core_fsm_1_REG;

END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_staller
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_staller IS
  PORT(
    core_wen : OUT STD_LOGIC;
    x1_rsci_wen_comp : IN STD_LOGIC;
    y1_rsci_wen_comp : IN STD_LOGIC;
    x2_rsci_wen_comp : IN STD_LOGIC;
    y2_rsci_wen_comp : IN STD_LOGIC;
    acc_rsci_wen_comp : IN STD_LOGIC
  );
END getMaxLine_core_staller;

ARCHITECTURE v1 OF getMaxLine_core_staller IS
  -- Default Constants

BEGIN
  core_wen <= x1_rsci_wen_comp AND y1_rsci_wen_comp AND x2_rsci_wen_comp AND y2_rsci_wen_comp
      AND acc_rsci_wen_comp;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0);
    core_wen : IN STD_LOGIC;
    T_LINE_if_if_dividend1_mul_cmp_z_oreg : OUT STD_LOGIC_VECTOR (43 DOWNTO 0)
  );
END getMaxLine_core_wait_dp;

ARCHITECTURE v1 OF getMaxLine_core_wait_dp IS
  -- Default Constants

BEGIN
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_if_dividend1_mul_cmp_z_oreg <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000000000000");
      ELSIF ( core_wen = '1' ) THEN
        T_LINE_if_if_dividend1_mul_cmp_z_oreg <= T_LINE_if_if_dividend1_mul_cmp_z;
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_acc_rsci_acc_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_acc_rsci_acc_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    acc_rsci_oswt : IN STD_LOGIC;
    acc_rsci_wen_comp : OUT STD_LOGIC;
    acc_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsci_biwt : IN STD_LOGIC;
    acc_rsci_bdwt : IN STD_LOGIC;
    acc_rsci_bcwt : OUT STD_LOGIC;
    acc_rsci_idat : IN STD_LOGIC_VECTOR (15 DOWNTO 0)
  );
END getMaxLine_core_acc_rsci_acc_wait_dp;

ARCHITECTURE v1 OF getMaxLine_core_acc_rsci_acc_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL acc_rsci_bcwt_drv : STD_LOGIC;

  -- Interconnect Declarations
  SIGNAL acc_rsci_idat_bfwt : STD_LOGIC_VECTOR (15 DOWNTO 0);

  FUNCTION MUX_v_16_2_2(input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

BEGIN
  -- Output Reader Assignments
  acc_rsci_bcwt <= acc_rsci_bcwt_drv;

  acc_rsci_wen_comp <= (NOT acc_rsci_oswt) OR acc_rsci_biwt OR acc_rsci_bcwt_drv;
  acc_rsci_idat_mxwt <= MUX_v_16_2_2(acc_rsci_idat, acc_rsci_idat_bfwt, acc_rsci_bcwt_drv);
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_rsci_bcwt_drv <= '0';
      ELSE
        acc_rsci_bcwt_drv <= NOT((NOT(acc_rsci_bcwt_drv OR acc_rsci_biwt)) OR acc_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_rsci_idat_bfwt <= STD_LOGIC_VECTOR'( "0000000000000000");
      ELSIF ( acc_rsci_biwt = '1' ) THEN
        acc_rsci_idat_bfwt <= acc_rsci_idat;
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_acc_rsci_acc_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_acc_rsci_acc_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    acc_rsci_oswt : IN STD_LOGIC;
    acc_rsci_biwt : OUT STD_LOGIC;
    acc_rsci_bdwt : OUT STD_LOGIC;
    acc_rsci_bcwt : IN STD_LOGIC;
    acc_rsci_irdy_core_sct : OUT STD_LOGIC;
    acc_rsci_ivld : IN STD_LOGIC
  );
END getMaxLine_core_acc_rsci_acc_wait_ctrl;

ARCHITECTURE v1 OF getMaxLine_core_acc_rsci_acc_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL acc_rsci_ogwt : STD_LOGIC;

BEGIN
  acc_rsci_bdwt <= acc_rsci_oswt AND core_wen;
  acc_rsci_biwt <= acc_rsci_ogwt AND acc_rsci_ivld;
  acc_rsci_ogwt <= acc_rsci_oswt AND (NOT acc_rsci_bcwt);
  acc_rsci_irdy_core_sct <= acc_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_y2_rsci_y2_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_y2_rsci_y2_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    y2_rsci_oswt : IN STD_LOGIC;
    y2_rsci_wen_comp : OUT STD_LOGIC;
    y2_rsci_biwt : IN STD_LOGIC;
    y2_rsci_bdwt : IN STD_LOGIC;
    y2_rsci_bcwt : OUT STD_LOGIC
  );
END getMaxLine_core_y2_rsci_y2_wait_dp;

ARCHITECTURE v1 OF getMaxLine_core_y2_rsci_y2_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL y2_rsci_bcwt_drv : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  y2_rsci_bcwt <= y2_rsci_bcwt_drv;

  y2_rsci_wen_comp <= (NOT y2_rsci_oswt) OR y2_rsci_biwt OR y2_rsci_bcwt_drv;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        y2_rsci_bcwt_drv <= '0';
      ELSE
        y2_rsci_bcwt_drv <= NOT((NOT(y2_rsci_bcwt_drv OR y2_rsci_biwt)) OR y2_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_y2_rsci_y2_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_y2_rsci_y2_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    y2_rsci_oswt : IN STD_LOGIC;
    y2_rsci_irdy : IN STD_LOGIC;
    y2_rsci_biwt : OUT STD_LOGIC;
    y2_rsci_bdwt : OUT STD_LOGIC;
    y2_rsci_bcwt : IN STD_LOGIC;
    y2_rsci_ivld_core_sct : OUT STD_LOGIC
  );
END getMaxLine_core_y2_rsci_y2_wait_ctrl;

ARCHITECTURE v1 OF getMaxLine_core_y2_rsci_y2_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL y2_rsci_ogwt : STD_LOGIC;

BEGIN
  y2_rsci_bdwt <= y2_rsci_oswt AND core_wen;
  y2_rsci_biwt <= y2_rsci_ogwt AND y2_rsci_irdy;
  y2_rsci_ogwt <= y2_rsci_oswt AND (NOT y2_rsci_bcwt);
  y2_rsci_ivld_core_sct <= y2_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_x2_rsci_x2_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_x2_rsci_x2_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    x2_rsci_oswt : IN STD_LOGIC;
    x2_rsci_wen_comp : OUT STD_LOGIC;
    x2_rsci_biwt : IN STD_LOGIC;
    x2_rsci_bdwt : IN STD_LOGIC;
    x2_rsci_bcwt : OUT STD_LOGIC
  );
END getMaxLine_core_x2_rsci_x2_wait_dp;

ARCHITECTURE v1 OF getMaxLine_core_x2_rsci_x2_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL x2_rsci_bcwt_drv : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  x2_rsci_bcwt <= x2_rsci_bcwt_drv;

  x2_rsci_wen_comp <= (NOT x2_rsci_oswt) OR x2_rsci_biwt OR x2_rsci_bcwt_drv;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x2_rsci_bcwt_drv <= '0';
      ELSE
        x2_rsci_bcwt_drv <= NOT((NOT(x2_rsci_bcwt_drv OR x2_rsci_biwt)) OR x2_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_x2_rsci_x2_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_x2_rsci_x2_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    x2_rsci_oswt : IN STD_LOGIC;
    x2_rsci_irdy : IN STD_LOGIC;
    x2_rsci_biwt : OUT STD_LOGIC;
    x2_rsci_bdwt : OUT STD_LOGIC;
    x2_rsci_bcwt : IN STD_LOGIC;
    x2_rsci_ivld_core_sct : OUT STD_LOGIC
  );
END getMaxLine_core_x2_rsci_x2_wait_ctrl;

ARCHITECTURE v1 OF getMaxLine_core_x2_rsci_x2_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL x2_rsci_ogwt : STD_LOGIC;

BEGIN
  x2_rsci_bdwt <= x2_rsci_oswt AND core_wen;
  x2_rsci_biwt <= x2_rsci_ogwt AND x2_rsci_irdy;
  x2_rsci_ogwt <= x2_rsci_oswt AND (NOT x2_rsci_bcwt);
  x2_rsci_ivld_core_sct <= x2_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_y1_rsci_y1_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_y1_rsci_y1_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    y1_rsci_oswt : IN STD_LOGIC;
    y1_rsci_wen_comp : OUT STD_LOGIC;
    y1_rsci_biwt : IN STD_LOGIC;
    y1_rsci_bdwt : IN STD_LOGIC;
    y1_rsci_bcwt : OUT STD_LOGIC
  );
END getMaxLine_core_y1_rsci_y1_wait_dp;

ARCHITECTURE v1 OF getMaxLine_core_y1_rsci_y1_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL y1_rsci_bcwt_drv : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  y1_rsci_bcwt <= y1_rsci_bcwt_drv;

  y1_rsci_wen_comp <= (NOT y1_rsci_oswt) OR y1_rsci_biwt OR y1_rsci_bcwt_drv;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        y1_rsci_bcwt_drv <= '0';
      ELSE
        y1_rsci_bcwt_drv <= NOT((NOT(y1_rsci_bcwt_drv OR y1_rsci_biwt)) OR y1_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_y1_rsci_y1_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_y1_rsci_y1_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    y1_rsci_oswt : IN STD_LOGIC;
    y1_rsci_irdy : IN STD_LOGIC;
    y1_rsci_biwt : OUT STD_LOGIC;
    y1_rsci_bdwt : OUT STD_LOGIC;
    y1_rsci_bcwt : IN STD_LOGIC;
    y1_rsci_ivld_core_sct : OUT STD_LOGIC
  );
END getMaxLine_core_y1_rsci_y1_wait_ctrl;

ARCHITECTURE v1 OF getMaxLine_core_y1_rsci_y1_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL y1_rsci_ogwt : STD_LOGIC;

BEGIN
  y1_rsci_bdwt <= y1_rsci_oswt AND core_wen;
  y1_rsci_biwt <= y1_rsci_ogwt AND y1_rsci_irdy;
  y1_rsci_ogwt <= y1_rsci_oswt AND (NOT y1_rsci_bcwt);
  y1_rsci_ivld_core_sct <= y1_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_x1_rsci_x1_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_x1_rsci_x1_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    x1_rsci_oswt : IN STD_LOGIC;
    x1_rsci_wen_comp : OUT STD_LOGIC;
    x1_rsci_biwt : IN STD_LOGIC;
    x1_rsci_bdwt : IN STD_LOGIC;
    x1_rsci_bcwt : OUT STD_LOGIC
  );
END getMaxLine_core_x1_rsci_x1_wait_dp;

ARCHITECTURE v1 OF getMaxLine_core_x1_rsci_x1_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL x1_rsci_bcwt_drv : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  x1_rsci_bcwt <= x1_rsci_bcwt_drv;

  x1_rsci_wen_comp <= (NOT x1_rsci_oswt) OR x1_rsci_biwt OR x1_rsci_bcwt_drv;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x1_rsci_bcwt_drv <= '0';
      ELSE
        x1_rsci_bcwt_drv <= NOT((NOT(x1_rsci_bcwt_drv OR x1_rsci_biwt)) OR x1_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_x1_rsci_x1_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_x1_rsci_x1_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    x1_rsci_oswt : IN STD_LOGIC;
    x1_rsci_irdy : IN STD_LOGIC;
    x1_rsci_biwt : OUT STD_LOGIC;
    x1_rsci_bdwt : OUT STD_LOGIC;
    x1_rsci_bcwt : IN STD_LOGIC;
    x1_rsci_ivld_core_sct : OUT STD_LOGIC
  );
END getMaxLine_core_x1_rsci_x1_wait_ctrl;

ARCHITECTURE v1 OF getMaxLine_core_x1_rsci_x1_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL x1_rsci_ogwt : STD_LOGIC;

BEGIN
  x1_rsci_bdwt <= x1_rsci_oswt AND core_wen;
  x1_rsci_biwt <= x1_rsci_ogwt AND x1_rsci_irdy;
  x1_rsci_ogwt <= x1_rsci_oswt AND (NOT x1_rsci_bcwt);
  x1_rsci_ivld_core_sct <= x1_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
    IS
  PORT(
    en : OUT STD_LOGIC;
    data_out : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
    we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    addr : OUT STD_LOGIC_VECTOR (37 DOWNTO 0);
    data_in : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
    data_in_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
    addr_d : IN STD_LOGIC_VECTOR (37 DOWNTO 0);
    re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    data_out_d : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
    en_d : IN STD_LOGIC
  );
END houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen;

ARCHITECTURE v1 OF houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
    IS
  -- Default Constants

BEGIN
  en <= en_d;
  data_out_d <= data_out;
  we <= we_d;
  re <= re_d;
  addr <= addr_d;
  data_in <= data_in_d;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_core_fsm
--  FSM Module
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_core_fsm IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    fsm_output : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
    acc_tmp_vinit_C_0_tr0 : IN STD_LOGIC;
    HCOL_C_0_tr0 : IN STD_LOGIC;
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
        : IN STD_LOGIC;
    HACC_C_15_tr0 : IN STD_LOGIC;
    HCOL_C_1_tr0 : IN STD_LOGIC;
    HROW_C_0_tr0 : IN STD_LOGIC;
    for_1_C_2_tr0 : IN STD_LOGIC
  );
END houghTransform_core_core_fsm;

ARCHITECTURE v1 OF houghTransform_core_core_fsm IS
  -- Default Constants

  -- FSM State Type Declaration for houghTransform_core_core_fsm_1
  TYPE houghTransform_core_core_fsm_1_ST IS (core_rlp_C_0, main_C_0, acc_tmp_vinit_C_0,
      HCOL_C_0, HACC_C_0, HACC_C_1, HACC_C_2, HACC_C_3, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5,
      HACC_C_4, HACC_C_5, HACC_C_6, HACC_C_7, HACC_C_8, HACC_C_9, HACC_C_10, HACC_C_11,
      HACC_C_12, HACC_C_13, HACC_C_14, HACC_C_15, HCOL_C_1, HROW_C_0, for_1_C_0,
      for_1_C_1, for_1_C_2, main_C_1);

  SIGNAL state_var : houghTransform_core_core_fsm_1_ST;
  SIGNAL state_var_NS : houghTransform_core_core_fsm_1_ST;

BEGIN
  houghTransform_core_core_fsm_1 : PROCESS (acc_tmp_vinit_C_0_tr0, HCOL_C_0_tr0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0,
      HACC_C_15_tr0, HCOL_C_1_tr0, HROW_C_0_tr0, for_1_C_2_tr0, state_var)
  BEGIN
    CASE state_var IS
      WHEN main_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000000010");
        state_var_NS <= acc_tmp_vinit_C_0;
      WHEN acc_tmp_vinit_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000000100");
        IF ( acc_tmp_vinit_C_0_tr0 = '1' ) THEN
          state_var_NS <= HCOL_C_0;
        ELSE
          state_var_NS <= acc_tmp_vinit_C_0;
        END IF;
      WHEN HCOL_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000001000");
        IF ( HCOL_C_0_tr0 = '1' ) THEN
          state_var_NS <= HCOL_C_1;
        ELSE
          state_var_NS <= HACC_C_0;
        END IF;
      WHEN HACC_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000010000");
        state_var_NS <= HACC_C_1;
      WHEN HACC_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000100000");
        state_var_NS <= HACC_C_2;
      WHEN HACC_C_2 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000001000000");
        state_var_NS <= HACC_C_3;
      WHEN HACC_C_3 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000010000000");
        state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
      WHEN ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000100000000");
        state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1;
      WHEN ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000001000000000");
        state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2;
      WHEN ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000010000000000");
        state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3;
      WHEN ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000100000000000");
        state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4;
      WHEN ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000001000000000000");
        state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5;
      WHEN ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000010000000000000");
        IF ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
            = '1' ) THEN
          state_var_NS <= HACC_C_4;
        ELSE
          state_var_NS <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        END IF;
      WHEN HACC_C_4 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000100000000000000");
        state_var_NS <= HACC_C_5;
      WHEN HACC_C_5 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000001000000000000000");
        state_var_NS <= HACC_C_6;
      WHEN HACC_C_6 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000010000000000000000");
        state_var_NS <= HACC_C_7;
      WHEN HACC_C_7 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000100000000000000000");
        state_var_NS <= HACC_C_8;
      WHEN HACC_C_8 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000001000000000000000000");
        state_var_NS <= HACC_C_9;
      WHEN HACC_C_9 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000010000000000000000000");
        state_var_NS <= HACC_C_10;
      WHEN HACC_C_10 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000100000000000000000000");
        state_var_NS <= HACC_C_11;
      WHEN HACC_C_11 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000001000000000000000000000");
        state_var_NS <= HACC_C_12;
      WHEN HACC_C_12 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000010000000000000000000000");
        state_var_NS <= HACC_C_13;
      WHEN HACC_C_13 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000100000000000000000000000");
        state_var_NS <= HACC_C_14;
      WHEN HACC_C_14 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000001000000000000000000000000");
        state_var_NS <= HACC_C_15;
      WHEN HACC_C_15 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000010000000000000000000000000");
        IF ( HACC_C_15_tr0 = '1' ) THEN
          state_var_NS <= HCOL_C_1;
        ELSE
          state_var_NS <= HACC_C_0;
        END IF;
      WHEN HCOL_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000100000000000000000000000000");
        IF ( HCOL_C_1_tr0 = '1' ) THEN
          state_var_NS <= HROW_C_0;
        ELSE
          state_var_NS <= HCOL_C_0;
        END IF;
      WHEN HROW_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00001000000000000000000000000000");
        IF ( HROW_C_0_tr0 = '1' ) THEN
          state_var_NS <= for_1_C_0;
        ELSE
          state_var_NS <= HCOL_C_0;
        END IF;
      WHEN for_1_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00010000000000000000000000000000");
        state_var_NS <= for_1_C_1;
      WHEN for_1_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "00100000000000000000000000000000");
        state_var_NS <= for_1_C_2;
      WHEN for_1_C_2 =>
        fsm_output <= STD_LOGIC_VECTOR'( "01000000000000000000000000000000");
        IF ( for_1_C_2_tr0 = '1' ) THEN
          state_var_NS <= main_C_1;
        ELSE
          state_var_NS <= for_1_C_0;
        END IF;
      WHEN main_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "10000000000000000000000000000000");
        state_var_NS <= main_C_0;
      -- core_rlp_C_0
      WHEN OTHERS =>
        fsm_output <= STD_LOGIC_VECTOR'( "00000000000000000000000000000001");
        state_var_NS <= main_C_0;
    END CASE;
  END PROCESS houghTransform_core_core_fsm_1;

  houghTransform_core_core_fsm_1_REG : PROCESS (clk)
  BEGIN
    IF clk'event AND ( clk = '1' ) THEN
      IF ( rst = '1' ) THEN
        state_var <= core_rlp_C_0;
      ELSE
        IF ( core_wen = '1' ) THEN
          state_var <= state_var_NS;
        END IF;
      END IF;
    END IF;
  END PROCESS houghTransform_core_core_fsm_1_REG;

END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_staller
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_staller IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    core_wen : OUT STD_LOGIC;
    core_wten : OUT STD_LOGIC;
    data_in_rsci_wen_comp : IN STD_LOGIC;
    acc_rsci_wen_comp : IN STD_LOGIC
  );
END houghTransform_core_staller;

ARCHITECTURE v1 OF houghTransform_core_staller IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL core_wen_drv : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  core_wen <= core_wen_drv;

  core_wen_drv <= data_in_rsci_wen_comp AND acc_rsci_wen_comp;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        core_wten <= '0';
      ELSE
        core_wten <= NOT core_wen_drv;
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
    IS
  PORT(
    core_wten : IN STD_LOGIC;
    heightIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC;
    heightIn_rsc_triosy_obj_ld_core_sct : OUT STD_LOGIC
  );
END houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl;

ARCHITECTURE v1 OF houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
    IS
  -- Default Constants

BEGIN
  heightIn_rsc_triosy_obj_ld_core_sct <= heightIn_rsc_triosy_obj_iswt0 AND (NOT core_wten);
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl IS
  PORT(
    core_wten : IN STD_LOGIC;
    widthIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC;
    widthIn_rsc_triosy_obj_ld_core_sct : OUT STD_LOGIC
  );
END houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl;

ARCHITECTURE v1 OF houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl
    IS
  -- Default Constants

BEGIN
  widthIn_rsc_triosy_obj_ld_core_sct <= widthIn_rsc_triosy_obj_iswt0 AND (NOT core_wten);
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    acc_tmp_rsc_cgo_iro : IN STD_LOGIC;
    acc_tmp_rsci_data_out_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
    acc_tmp_rsci_en_d : OUT STD_LOGIC;
    core_wen : IN STD_LOGIC;
    acc_tmp_rsc_cgo : IN STD_LOGIC;
    acc_tmp_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
  );
END houghTransform_core_wait_dp;

ARCHITECTURE v1 OF houghTransform_core_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL acc_tmp_rsci_en_d_drv : STD_LOGIC;

  -- Interconnect Declarations
  SIGNAL acc_tmp_rsci_data_out_d_oreg_pconst_15_0 : STD_LOGIC_VECTOR (15 DOWNTO 0);

BEGIN
  -- Output Reader Assignments
  acc_tmp_rsci_en_d <= acc_tmp_rsci_en_d_drv;

  acc_tmp_rsci_en_d_drv <= NOT(core_wen AND (acc_tmp_rsc_cgo OR acc_tmp_rsc_cgo_iro));
  acc_tmp_rsci_data_out_d_oreg <= acc_tmp_rsci_data_out_d_oreg_pconst_15_0;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_tmp_rsci_data_out_d_oreg_pconst_15_0 <= STD_LOGIC_VECTOR'( "0000000000000000");
      ELSIF ( acc_tmp_rsci_en_d_drv = '0' ) THEN
        acc_tmp_rsci_data_out_d_oreg_pconst_15_0 <= acc_tmp_rsci_data_out_d(15 DOWNTO
            0);
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_acc_rsci_acc_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_acc_rsci_acc_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    acc_rsci_oswt : IN STD_LOGIC;
    acc_rsci_wen_comp : OUT STD_LOGIC;
    acc_rsci_biwt : IN STD_LOGIC;
    acc_rsci_bdwt : IN STD_LOGIC;
    acc_rsci_bcwt : OUT STD_LOGIC
  );
END houghTransform_core_acc_rsci_acc_wait_dp;

ARCHITECTURE v1 OF houghTransform_core_acc_rsci_acc_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL acc_rsci_bcwt_drv : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  acc_rsci_bcwt <= acc_rsci_bcwt_drv;

  acc_rsci_wen_comp <= (NOT acc_rsci_oswt) OR acc_rsci_biwt OR acc_rsci_bcwt_drv;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_rsci_bcwt_drv <= '0';
      ELSE
        acc_rsci_bcwt_drv <= NOT((NOT(acc_rsci_bcwt_drv OR acc_rsci_biwt)) OR acc_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_acc_rsci_acc_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_acc_rsci_acc_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    acc_rsci_oswt : IN STD_LOGIC;
    acc_rsci_irdy : IN STD_LOGIC;
    acc_rsci_biwt : OUT STD_LOGIC;
    acc_rsci_bdwt : OUT STD_LOGIC;
    acc_rsci_bcwt : IN STD_LOGIC;
    acc_rsci_ivld_core_sct : OUT STD_LOGIC
  );
END houghTransform_core_acc_rsci_acc_wait_ctrl;

ARCHITECTURE v1 OF houghTransform_core_acc_rsci_acc_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL acc_rsci_ogwt : STD_LOGIC;

BEGIN
  acc_rsci_bdwt <= acc_rsci_oswt AND core_wen;
  acc_rsci_biwt <= acc_rsci_ogwt AND acc_rsci_irdy;
  acc_rsci_ogwt <= acc_rsci_oswt AND (NOT acc_rsci_bcwt);
  acc_rsci_ivld_core_sct <= acc_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_data_in_rsci_data_in_wait_dp
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_data_in_rsci_data_in_wait_dp IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    data_in_rsci_oswt : IN STD_LOGIC;
    data_in_rsci_wen_comp : OUT STD_LOGIC;
    data_in_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    data_in_rsci_biwt : IN STD_LOGIC;
    data_in_rsci_bdwt : IN STD_LOGIC;
    data_in_rsci_bcwt : OUT STD_LOGIC;
    data_in_rsci_idat : IN STD_LOGIC_VECTOR (7 DOWNTO 0)
  );
END houghTransform_core_data_in_rsci_data_in_wait_dp;

ARCHITECTURE v1 OF houghTransform_core_data_in_rsci_data_in_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL data_in_rsci_bcwt_drv : STD_LOGIC;

  -- Interconnect Declarations
  SIGNAL data_in_rsci_idat_bfwt : STD_LOGIC_VECTOR (7 DOWNTO 0);

  FUNCTION MUX_v_8_2_2(input_0 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(7 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

BEGIN
  -- Output Reader Assignments
  data_in_rsci_bcwt <= data_in_rsci_bcwt_drv;

  data_in_rsci_wen_comp <= (NOT data_in_rsci_oswt) OR data_in_rsci_biwt OR data_in_rsci_bcwt_drv;
  data_in_rsci_idat_mxwt <= MUX_v_8_2_2(data_in_rsci_idat, data_in_rsci_idat_bfwt,
      data_in_rsci_bcwt_drv);
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        data_in_rsci_bcwt_drv <= '0';
      ELSE
        data_in_rsci_bcwt_drv <= NOT((NOT(data_in_rsci_bcwt_drv OR data_in_rsci_biwt))
            OR data_in_rsci_bdwt);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        data_in_rsci_idat_bfwt <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( data_in_rsci_biwt = '1' ) THEN
        data_in_rsci_idat_bfwt <= data_in_rsci_idat;
      END IF;
    END IF;
  END PROCESS;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_data_in_rsci_data_in_wait_ctrl
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_data_in_rsci_data_in_wait_ctrl IS
  PORT(
    core_wen : IN STD_LOGIC;
    data_in_rsci_oswt : IN STD_LOGIC;
    data_in_rsci_biwt : OUT STD_LOGIC;
    data_in_rsci_bdwt : OUT STD_LOGIC;
    data_in_rsci_bcwt : IN STD_LOGIC;
    data_in_rsci_irdy_core_sct : OUT STD_LOGIC;
    data_in_rsci_ivld : IN STD_LOGIC
  );
END houghTransform_core_data_in_rsci_data_in_wait_ctrl;

ARCHITECTURE v1 OF houghTransform_core_data_in_rsci_data_in_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL data_in_rsci_ogwt : STD_LOGIC;

BEGIN
  data_in_rsci_bdwt <= data_in_rsci_oswt AND core_wen;
  data_in_rsci_biwt <= data_in_rsci_ogwt AND data_in_rsci_ivld;
  data_in_rsci_ogwt <= data_in_rsci_oswt AND (NOT data_in_rsci_bcwt);
  data_in_rsci_irdy_core_sct <= data_in_rsci_ogwt;
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_acc_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_acc_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    acc_rsc_dat : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsc_vld : IN STD_LOGIC;
    acc_rsc_rdy : OUT STD_LOGIC;
    core_wen : IN STD_LOGIC;
    acc_rsci_oswt : IN STD_LOGIC;
    acc_rsci_wen_comp : OUT STD_LOGIC;
    acc_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
  );
END getMaxLine_core_acc_rsci;

ARCHITECTURE v1 OF getMaxLine_core_acc_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL acc_rsci_biwt : STD_LOGIC;
  SIGNAL acc_rsci_bdwt : STD_LOGIC;
  SIGNAL acc_rsci_bcwt : STD_LOGIC;
  SIGNAL acc_rsci_irdy_core_sct : STD_LOGIC;
  SIGNAL acc_rsci_ivld : STD_LOGIC;
  SIGNAL acc_rsci_idat : STD_LOGIC_VECTOR (15 DOWNTO 0);

  SIGNAL acc_rsci_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_rsci_idat_1 : STD_LOGIC_VECTOR (15 DOWNTO 0);

  COMPONENT getMaxLine_core_acc_rsci_acc_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      acc_rsci_oswt : IN STD_LOGIC;
      acc_rsci_biwt : OUT STD_LOGIC;
      acc_rsci_bdwt : OUT STD_LOGIC;
      acc_rsci_bcwt : IN STD_LOGIC;
      acc_rsci_irdy_core_sct : OUT STD_LOGIC;
      acc_rsci_ivld : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT getMaxLine_core_acc_rsci_acc_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      acc_rsci_oswt : IN STD_LOGIC;
      acc_rsci_wen_comp : OUT STD_LOGIC;
      acc_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsci_biwt : IN STD_LOGIC;
      acc_rsci_bdwt : IN STD_LOGIC;
      acc_rsci_bcwt : OUT STD_LOGIC;
      acc_rsci_idat : IN STD_LOGIC_VECTOR (15 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_acc_rsci_acc_wait_dp_inst_acc_rsci_idat_mxwt : STD_LOGIC_VECTOR
      (15 DOWNTO 0);
  SIGNAL getMaxLine_core_acc_rsci_acc_wait_dp_inst_acc_rsci_idat : STD_LOGIC_VECTOR
      (15 DOWNTO 0);

BEGIN
  acc_rsci : work.ccs_in_wait_pkg_v1.ccs_in_wait_v1
    GENERIC MAP(
      rscid => 19,
      width => 16
      )
    PORT MAP(
      rdy => acc_rsc_rdy,
      vld => acc_rsc_vld,
      dat => acc_rsci_dat,
      irdy => acc_rsci_irdy_core_sct,
      ivld => acc_rsci_ivld,
      idat => acc_rsci_idat_1
    );
  acc_rsci_dat <= acc_rsc_dat;
  acc_rsci_idat <= acc_rsci_idat_1;

  getMaxLine_core_acc_rsci_acc_wait_ctrl_inst : getMaxLine_core_acc_rsci_acc_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      acc_rsci_oswt => acc_rsci_oswt,
      acc_rsci_biwt => acc_rsci_biwt,
      acc_rsci_bdwt => acc_rsci_bdwt,
      acc_rsci_bcwt => acc_rsci_bcwt,
      acc_rsci_irdy_core_sct => acc_rsci_irdy_core_sct,
      acc_rsci_ivld => acc_rsci_ivld
    );
  getMaxLine_core_acc_rsci_acc_wait_dp_inst : getMaxLine_core_acc_rsci_acc_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      acc_rsci_oswt => acc_rsci_oswt,
      acc_rsci_wen_comp => acc_rsci_wen_comp,
      acc_rsci_idat_mxwt => getMaxLine_core_acc_rsci_acc_wait_dp_inst_acc_rsci_idat_mxwt,
      acc_rsci_biwt => acc_rsci_biwt,
      acc_rsci_bdwt => acc_rsci_bdwt,
      acc_rsci_bcwt => acc_rsci_bcwt,
      acc_rsci_idat => getMaxLine_core_acc_rsci_acc_wait_dp_inst_acc_rsci_idat
    );
  acc_rsci_idat_mxwt <= getMaxLine_core_acc_rsci_acc_wait_dp_inst_acc_rsci_idat_mxwt;
  getMaxLine_core_acc_rsci_acc_wait_dp_inst_acc_rsci_idat <= acc_rsci_idat;

END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_y2_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_y2_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y2_rsc_vld : OUT STD_LOGIC;
    y2_rsc_rdy : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    y2_rsci_oswt : IN STD_LOGIC;
    y2_rsci_wen_comp : OUT STD_LOGIC;
    y2_rsci_idat : IN STD_LOGIC_VECTOR (9 DOWNTO 0)
  );
END getMaxLine_core_y2_rsci;

ARCHITECTURE v1 OF getMaxLine_core_y2_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL y2_rsci_irdy : STD_LOGIC;
  SIGNAL y2_rsci_biwt : STD_LOGIC;
  SIGNAL y2_rsci_bdwt : STD_LOGIC;
  SIGNAL y2_rsci_bcwt : STD_LOGIC;
  SIGNAL y2_rsci_ivld_core_sct : STD_LOGIC;

  SIGNAL y2_rsci_idat_1 : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL y2_rsci_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);

  COMPONENT getMaxLine_core_y2_rsci_y2_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      y2_rsci_oswt : IN STD_LOGIC;
      y2_rsci_irdy : IN STD_LOGIC;
      y2_rsci_biwt : OUT STD_LOGIC;
      y2_rsci_bdwt : OUT STD_LOGIC;
      y2_rsci_bcwt : IN STD_LOGIC;
      y2_rsci_ivld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
  COMPONENT getMaxLine_core_y2_rsci_y2_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      y2_rsci_oswt : IN STD_LOGIC;
      y2_rsci_wen_comp : OUT STD_LOGIC;
      y2_rsci_biwt : IN STD_LOGIC;
      y2_rsci_bdwt : IN STD_LOGIC;
      y2_rsci_bcwt : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  y2_rsci : work.ccs_out_wait_pkg_v1.ccs_out_wait_v1
    GENERIC MAP(
      rscid => 18,
      width => 10
      )
    PORT MAP(
      irdy => y2_rsci_irdy,
      ivld => y2_rsci_ivld_core_sct,
      idat => y2_rsci_idat_1,
      rdy => y2_rsc_rdy,
      vld => y2_rsc_vld,
      dat => y2_rsci_dat
    );
  y2_rsci_idat_1 <= y2_rsci_idat;
  y2_rsc_dat <= y2_rsci_dat;

  getMaxLine_core_y2_rsci_y2_wait_ctrl_inst : getMaxLine_core_y2_rsci_y2_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      y2_rsci_oswt => y2_rsci_oswt,
      y2_rsci_irdy => y2_rsci_irdy,
      y2_rsci_biwt => y2_rsci_biwt,
      y2_rsci_bdwt => y2_rsci_bdwt,
      y2_rsci_bcwt => y2_rsci_bcwt,
      y2_rsci_ivld_core_sct => y2_rsci_ivld_core_sct
    );
  getMaxLine_core_y2_rsci_y2_wait_dp_inst : getMaxLine_core_y2_rsci_y2_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      y2_rsci_oswt => y2_rsci_oswt,
      y2_rsci_wen_comp => y2_rsci_wen_comp,
      y2_rsci_biwt => y2_rsci_biwt,
      y2_rsci_bdwt => y2_rsci_bdwt,
      y2_rsci_bcwt => y2_rsci_bcwt
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_x2_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_x2_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x2_rsc_vld : OUT STD_LOGIC;
    x2_rsc_rdy : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    x2_rsci_oswt : IN STD_LOGIC;
    x2_rsci_wen_comp : OUT STD_LOGIC;
    x2_rsci_idat : IN STD_LOGIC_VECTOR (10 DOWNTO 0)
  );
END getMaxLine_core_x2_rsci;

ARCHITECTURE v1 OF getMaxLine_core_x2_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL x2_rsci_irdy : STD_LOGIC;
  SIGNAL x2_rsci_biwt : STD_LOGIC;
  SIGNAL x2_rsci_bdwt : STD_LOGIC;
  SIGNAL x2_rsci_bcwt : STD_LOGIC;
  SIGNAL x2_rsci_ivld_core_sct : STD_LOGIC;

  SIGNAL x2_rsci_idat_1 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL x2_rsci_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);

  COMPONENT getMaxLine_core_x2_rsci_x2_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      x2_rsci_oswt : IN STD_LOGIC;
      x2_rsci_irdy : IN STD_LOGIC;
      x2_rsci_biwt : OUT STD_LOGIC;
      x2_rsci_bdwt : OUT STD_LOGIC;
      x2_rsci_bcwt : IN STD_LOGIC;
      x2_rsci_ivld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
  COMPONENT getMaxLine_core_x2_rsci_x2_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      x2_rsci_oswt : IN STD_LOGIC;
      x2_rsci_wen_comp : OUT STD_LOGIC;
      x2_rsci_biwt : IN STD_LOGIC;
      x2_rsci_bdwt : IN STD_LOGIC;
      x2_rsci_bcwt : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  x2_rsci : work.ccs_out_wait_pkg_v1.ccs_out_wait_v1
    GENERIC MAP(
      rscid => 17,
      width => 11
      )
    PORT MAP(
      irdy => x2_rsci_irdy,
      ivld => x2_rsci_ivld_core_sct,
      idat => x2_rsci_idat_1,
      rdy => x2_rsc_rdy,
      vld => x2_rsc_vld,
      dat => x2_rsci_dat
    );
  x2_rsci_idat_1 <= x2_rsci_idat;
  x2_rsc_dat <= x2_rsci_dat;

  getMaxLine_core_x2_rsci_x2_wait_ctrl_inst : getMaxLine_core_x2_rsci_x2_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      x2_rsci_oswt => x2_rsci_oswt,
      x2_rsci_irdy => x2_rsci_irdy,
      x2_rsci_biwt => x2_rsci_biwt,
      x2_rsci_bdwt => x2_rsci_bdwt,
      x2_rsci_bcwt => x2_rsci_bcwt,
      x2_rsci_ivld_core_sct => x2_rsci_ivld_core_sct
    );
  getMaxLine_core_x2_rsci_x2_wait_dp_inst : getMaxLine_core_x2_rsci_x2_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      x2_rsci_oswt => x2_rsci_oswt,
      x2_rsci_wen_comp => x2_rsci_wen_comp,
      x2_rsci_biwt => x2_rsci_biwt,
      x2_rsci_bdwt => x2_rsci_bdwt,
      x2_rsci_bcwt => x2_rsci_bcwt
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_y1_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_y1_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y1_rsc_vld : OUT STD_LOGIC;
    y1_rsc_rdy : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    y1_rsci_oswt : IN STD_LOGIC;
    y1_rsci_wen_comp : OUT STD_LOGIC;
    y1_rsci_idat : IN STD_LOGIC_VECTOR (9 DOWNTO 0)
  );
END getMaxLine_core_y1_rsci;

ARCHITECTURE v1 OF getMaxLine_core_y1_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL y1_rsci_irdy : STD_LOGIC;
  SIGNAL y1_rsci_biwt : STD_LOGIC;
  SIGNAL y1_rsci_bdwt : STD_LOGIC;
  SIGNAL y1_rsci_bcwt : STD_LOGIC;
  SIGNAL y1_rsci_ivld_core_sct : STD_LOGIC;

  SIGNAL y1_rsci_idat_1 : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL y1_rsci_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);

  COMPONENT getMaxLine_core_y1_rsci_y1_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      y1_rsci_oswt : IN STD_LOGIC;
      y1_rsci_irdy : IN STD_LOGIC;
      y1_rsci_biwt : OUT STD_LOGIC;
      y1_rsci_bdwt : OUT STD_LOGIC;
      y1_rsci_bcwt : IN STD_LOGIC;
      y1_rsci_ivld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
  COMPONENT getMaxLine_core_y1_rsci_y1_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      y1_rsci_oswt : IN STD_LOGIC;
      y1_rsci_wen_comp : OUT STD_LOGIC;
      y1_rsci_biwt : IN STD_LOGIC;
      y1_rsci_bdwt : IN STD_LOGIC;
      y1_rsci_bcwt : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  y1_rsci : work.ccs_out_wait_pkg_v1.ccs_out_wait_v1
    GENERIC MAP(
      rscid => 16,
      width => 10
      )
    PORT MAP(
      irdy => y1_rsci_irdy,
      ivld => y1_rsci_ivld_core_sct,
      idat => y1_rsci_idat_1,
      rdy => y1_rsc_rdy,
      vld => y1_rsc_vld,
      dat => y1_rsci_dat
    );
  y1_rsci_idat_1 <= y1_rsci_idat;
  y1_rsc_dat <= y1_rsci_dat;

  getMaxLine_core_y1_rsci_y1_wait_ctrl_inst : getMaxLine_core_y1_rsci_y1_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      y1_rsci_oswt => y1_rsci_oswt,
      y1_rsci_irdy => y1_rsci_irdy,
      y1_rsci_biwt => y1_rsci_biwt,
      y1_rsci_bdwt => y1_rsci_bdwt,
      y1_rsci_bcwt => y1_rsci_bcwt,
      y1_rsci_ivld_core_sct => y1_rsci_ivld_core_sct
    );
  getMaxLine_core_y1_rsci_y1_wait_dp_inst : getMaxLine_core_y1_rsci_y1_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      y1_rsci_oswt => y1_rsci_oswt,
      y1_rsci_wen_comp => y1_rsci_wen_comp,
      y1_rsci_biwt => y1_rsci_biwt,
      y1_rsci_bdwt => y1_rsci_bdwt,
      y1_rsci_bcwt => y1_rsci_bcwt
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core_x1_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core_x1_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x1_rsc_vld : OUT STD_LOGIC;
    x1_rsc_rdy : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    x1_rsci_oswt : IN STD_LOGIC;
    x1_rsci_wen_comp : OUT STD_LOGIC;
    x1_rsci_idat : IN STD_LOGIC_VECTOR (10 DOWNTO 0)
  );
END getMaxLine_core_x1_rsci;

ARCHITECTURE v1 OF getMaxLine_core_x1_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL x1_rsci_irdy : STD_LOGIC;
  SIGNAL x1_rsci_biwt : STD_LOGIC;
  SIGNAL x1_rsci_bdwt : STD_LOGIC;
  SIGNAL x1_rsci_bcwt : STD_LOGIC;
  SIGNAL x1_rsci_ivld_core_sct : STD_LOGIC;

  SIGNAL x1_rsci_idat_1 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL x1_rsci_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);

  COMPONENT getMaxLine_core_x1_rsci_x1_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      x1_rsci_oswt : IN STD_LOGIC;
      x1_rsci_irdy : IN STD_LOGIC;
      x1_rsci_biwt : OUT STD_LOGIC;
      x1_rsci_bdwt : OUT STD_LOGIC;
      x1_rsci_bcwt : IN STD_LOGIC;
      x1_rsci_ivld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
  COMPONENT getMaxLine_core_x1_rsci_x1_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      x1_rsci_oswt : IN STD_LOGIC;
      x1_rsci_wen_comp : OUT STD_LOGIC;
      x1_rsci_biwt : IN STD_LOGIC;
      x1_rsci_bdwt : IN STD_LOGIC;
      x1_rsci_bcwt : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  x1_rsci : work.ccs_out_wait_pkg_v1.ccs_out_wait_v1
    GENERIC MAP(
      rscid => 15,
      width => 11
      )
    PORT MAP(
      irdy => x1_rsci_irdy,
      ivld => x1_rsci_ivld_core_sct,
      idat => x1_rsci_idat_1,
      rdy => x1_rsc_rdy,
      vld => x1_rsc_vld,
      dat => x1_rsci_dat
    );
  x1_rsci_idat_1 <= x1_rsci_idat;
  x1_rsc_dat <= x1_rsci_dat;

  getMaxLine_core_x1_rsci_x1_wait_ctrl_inst : getMaxLine_core_x1_rsci_x1_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      x1_rsci_oswt => x1_rsci_oswt,
      x1_rsci_irdy => x1_rsci_irdy,
      x1_rsci_biwt => x1_rsci_biwt,
      x1_rsci_bdwt => x1_rsci_bdwt,
      x1_rsci_bcwt => x1_rsci_bcwt,
      x1_rsci_ivld_core_sct => x1_rsci_ivld_core_sct
    );
  getMaxLine_core_x1_rsci_x1_wait_dp_inst : getMaxLine_core_x1_rsci_x1_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      x1_rsci_oswt => x1_rsci_oswt,
      x1_rsci_wen_comp => x1_rsci_wen_comp,
      x1_rsci_biwt => x1_rsci_biwt,
      x1_rsci_bdwt => x1_rsci_bdwt,
      x1_rsci_bcwt => x1_rsci_bcwt
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_heightIn_rsc_triosy_obj
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_heightIn_rsc_triosy_obj IS
  PORT(
    heightIn_rsc_triosy_lz : OUT STD_LOGIC;
    core_wten : IN STD_LOGIC;
    heightIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC
  );
END houghTransform_core_heightIn_rsc_triosy_obj;

ARCHITECTURE v1 OF houghTransform_core_heightIn_rsc_triosy_obj IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL heightIn_rsc_triosy_obj_ld_core_sct : STD_LOGIC;

  COMPONENT houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
    PORT(
      core_wten : IN STD_LOGIC;
      heightIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC;
      heightIn_rsc_triosy_obj_ld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  heightIn_rsc_triosy_obj : work.mgc_io_sync_pkg_v2.mgc_io_sync_v2
    GENERIC MAP(
      valid => 0
      )
    PORT MAP(
      ld => heightIn_rsc_triosy_obj_ld_core_sct,
      lz => heightIn_rsc_triosy_lz
    );
  houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl_inst
      : houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
    PORT MAP(
      core_wten => core_wten,
      heightIn_rsc_triosy_obj_iswt0 => heightIn_rsc_triosy_obj_iswt0,
      heightIn_rsc_triosy_obj_ld_core_sct => heightIn_rsc_triosy_obj_ld_core_sct
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_widthIn_rsc_triosy_obj
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_widthIn_rsc_triosy_obj IS
  PORT(
    widthIn_rsc_triosy_lz : OUT STD_LOGIC;
    core_wten : IN STD_LOGIC;
    widthIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC
  );
END houghTransform_core_widthIn_rsc_triosy_obj;

ARCHITECTURE v1 OF houghTransform_core_widthIn_rsc_triosy_obj IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL widthIn_rsc_triosy_obj_ld_core_sct : STD_LOGIC;

  COMPONENT houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl
    PORT(
      core_wten : IN STD_LOGIC;
      widthIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC;
      widthIn_rsc_triosy_obj_ld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  widthIn_rsc_triosy_obj : work.mgc_io_sync_pkg_v2.mgc_io_sync_v2
    GENERIC MAP(
      valid => 0
      )
    PORT MAP(
      ld => widthIn_rsc_triosy_obj_ld_core_sct,
      lz => widthIn_rsc_triosy_lz
    );
  houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl_inst :
      houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl
    PORT MAP(
      core_wten => core_wten,
      widthIn_rsc_triosy_obj_iswt0 => widthIn_rsc_triosy_obj_iswt0,
      widthIn_rsc_triosy_obj_ld_core_sct => widthIn_rsc_triosy_obj_ld_core_sct
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_acc_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_acc_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    acc_rsc_dat : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsc_vld : OUT STD_LOGIC;
    acc_rsc_rdy : IN STD_LOGIC;
    core_wen : IN STD_LOGIC;
    acc_rsci_oswt : IN STD_LOGIC;
    acc_rsci_wen_comp : OUT STD_LOGIC;
    acc_rsci_idat : IN STD_LOGIC_VECTOR (15 DOWNTO 0)
  );
END houghTransform_core_acc_rsci;

ARCHITECTURE v1 OF houghTransform_core_acc_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL acc_rsci_irdy : STD_LOGIC;
  SIGNAL acc_rsci_biwt : STD_LOGIC;
  SIGNAL acc_rsci_bdwt : STD_LOGIC;
  SIGNAL acc_rsci_bcwt : STD_LOGIC;
  SIGNAL acc_rsci_ivld_core_sct : STD_LOGIC;

  SIGNAL acc_rsci_idat_1 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_rsci_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);

  COMPONENT houghTransform_core_acc_rsci_acc_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      acc_rsci_oswt : IN STD_LOGIC;
      acc_rsci_irdy : IN STD_LOGIC;
      acc_rsci_biwt : OUT STD_LOGIC;
      acc_rsci_bdwt : OUT STD_LOGIC;
      acc_rsci_bcwt : IN STD_LOGIC;
      acc_rsci_ivld_core_sct : OUT STD_LOGIC
    );
  END COMPONENT;
  COMPONENT houghTransform_core_acc_rsci_acc_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      acc_rsci_oswt : IN STD_LOGIC;
      acc_rsci_wen_comp : OUT STD_LOGIC;
      acc_rsci_biwt : IN STD_LOGIC;
      acc_rsci_bdwt : IN STD_LOGIC;
      acc_rsci_bcwt : OUT STD_LOGIC
    );
  END COMPONENT;
BEGIN
  acc_rsci : work.ccs_out_wait_pkg_v1.ccs_out_wait_v1
    GENERIC MAP(
      rscid => 12,
      width => 16
      )
    PORT MAP(
      irdy => acc_rsci_irdy,
      ivld => acc_rsci_ivld_core_sct,
      idat => acc_rsci_idat_1,
      rdy => acc_rsc_rdy,
      vld => acc_rsc_vld,
      dat => acc_rsci_dat
    );
  acc_rsci_idat_1 <= acc_rsci_idat;
  acc_rsc_dat <= acc_rsci_dat;

  houghTransform_core_acc_rsci_acc_wait_ctrl_inst : houghTransform_core_acc_rsci_acc_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      acc_rsci_oswt => acc_rsci_oswt,
      acc_rsci_irdy => acc_rsci_irdy,
      acc_rsci_biwt => acc_rsci_biwt,
      acc_rsci_bdwt => acc_rsci_bdwt,
      acc_rsci_bcwt => acc_rsci_bcwt,
      acc_rsci_ivld_core_sct => acc_rsci_ivld_core_sct
    );
  houghTransform_core_acc_rsci_acc_wait_dp_inst : houghTransform_core_acc_rsci_acc_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      acc_rsci_oswt => acc_rsci_oswt,
      acc_rsci_wen_comp => acc_rsci_wen_comp,
      acc_rsci_biwt => acc_rsci_biwt,
      acc_rsci_bdwt => acc_rsci_bdwt,
      acc_rsci_bcwt => acc_rsci_bcwt
    );
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core_data_in_rsci
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core_data_in_rsci IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    data_in_rsc_vld : IN STD_LOGIC;
    data_in_rsc_rdy : OUT STD_LOGIC;
    core_wen : IN STD_LOGIC;
    data_in_rsci_oswt : IN STD_LOGIC;
    data_in_rsci_wen_comp : OUT STD_LOGIC;
    data_in_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
  );
END houghTransform_core_data_in_rsci;

ARCHITECTURE v1 OF houghTransform_core_data_in_rsci IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL data_in_rsci_biwt : STD_LOGIC;
  SIGNAL data_in_rsci_bdwt : STD_LOGIC;
  SIGNAL data_in_rsci_bcwt : STD_LOGIC;
  SIGNAL data_in_rsci_irdy_core_sct : STD_LOGIC;
  SIGNAL data_in_rsci_ivld : STD_LOGIC;
  SIGNAL data_in_rsci_idat : STD_LOGIC_VECTOR (7 DOWNTO 0);

  SIGNAL data_in_rsci_dat : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL data_in_rsci_idat_1 : STD_LOGIC_VECTOR (7 DOWNTO 0);

  COMPONENT houghTransform_core_data_in_rsci_data_in_wait_ctrl
    PORT(
      core_wen : IN STD_LOGIC;
      data_in_rsci_oswt : IN STD_LOGIC;
      data_in_rsci_biwt : OUT STD_LOGIC;
      data_in_rsci_bdwt : OUT STD_LOGIC;
      data_in_rsci_bcwt : IN STD_LOGIC;
      data_in_rsci_irdy_core_sct : OUT STD_LOGIC;
      data_in_rsci_ivld : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT houghTransform_core_data_in_rsci_data_in_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      data_in_rsci_oswt : IN STD_LOGIC;
      data_in_rsci_wen_comp : OUT STD_LOGIC;
      data_in_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
      data_in_rsci_biwt : IN STD_LOGIC;
      data_in_rsci_bdwt : IN STD_LOGIC;
      data_in_rsci_bcwt : OUT STD_LOGIC;
      data_in_rsci_idat : IN STD_LOGIC_VECTOR (7 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL houghTransform_core_data_in_rsci_data_in_wait_dp_inst_data_in_rsci_idat_mxwt
      : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL houghTransform_core_data_in_rsci_data_in_wait_dp_inst_data_in_rsci_idat
      : STD_LOGIC_VECTOR (7 DOWNTO 0);

BEGIN
  data_in_rsci : work.ccs_in_wait_pkg_v1.ccs_in_wait_v1
    GENERIC MAP(
      rscid => 9,
      width => 8
      )
    PORT MAP(
      rdy => data_in_rsc_rdy,
      vld => data_in_rsc_vld,
      dat => data_in_rsci_dat,
      irdy => data_in_rsci_irdy_core_sct,
      ivld => data_in_rsci_ivld,
      idat => data_in_rsci_idat_1
    );
  data_in_rsci_dat <= data_in_rsc_dat;
  data_in_rsci_idat <= data_in_rsci_idat_1;

  houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst : houghTransform_core_data_in_rsci_data_in_wait_ctrl
    PORT MAP(
      core_wen => core_wen,
      data_in_rsci_oswt => data_in_rsci_oswt,
      data_in_rsci_biwt => data_in_rsci_biwt,
      data_in_rsci_bdwt => data_in_rsci_bdwt,
      data_in_rsci_bcwt => data_in_rsci_bcwt,
      data_in_rsci_irdy_core_sct => data_in_rsci_irdy_core_sct,
      data_in_rsci_ivld => data_in_rsci_ivld
    );
  houghTransform_core_data_in_rsci_data_in_wait_dp_inst : houghTransform_core_data_in_rsci_data_in_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      data_in_rsci_oswt => data_in_rsci_oswt,
      data_in_rsci_wen_comp => data_in_rsci_wen_comp,
      data_in_rsci_idat_mxwt => houghTransform_core_data_in_rsci_data_in_wait_dp_inst_data_in_rsci_idat_mxwt,
      data_in_rsci_biwt => data_in_rsci_biwt,
      data_in_rsci_bdwt => data_in_rsci_bdwt,
      data_in_rsci_bcwt => data_in_rsci_bcwt,
      data_in_rsci_idat => houghTransform_core_data_in_rsci_data_in_wait_dp_inst_data_in_rsci_idat
    );
  data_in_rsci_idat_mxwt <= houghTransform_core_data_in_rsci_data_in_wait_dp_inst_data_in_rsci_idat_mxwt;
  houghTransform_core_data_in_rsci_data_in_wait_dp_inst_data_in_rsci_idat <= data_in_rsci_idat;

END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_core
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine_core IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x1_rsc_vld : OUT STD_LOGIC;
    x1_rsc_rdy : IN STD_LOGIC;
    y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y1_rsc_vld : OUT STD_LOGIC;
    y1_rsc_rdy : IN STD_LOGIC;
    x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x2_rsc_vld : OUT STD_LOGIC;
    x2_rsc_rdy : IN STD_LOGIC;
    y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y2_rsc_vld : OUT STD_LOGIC;
    y2_rsc_rdy : IN STD_LOGIC;
    acc_rsc_dat : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsc_vld : IN STD_LOGIC;
    acc_rsc_rdy : OUT STD_LOGIC;
    T_LINE_if_if_dividend1_mul_cmp_a : OUT STD_LOGIC_VECTOR (19 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_b : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0)
  );
END getMaxLine_core;

ARCHITECTURE v1 OF getMaxLine_core IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL core_wen : STD_LOGIC;
  SIGNAL x1_rsci_wen_comp : STD_LOGIC;
  SIGNAL y1_rsci_wen_comp : STD_LOGIC;
  SIGNAL x2_rsci_wen_comp : STD_LOGIC;
  SIGNAL y2_rsci_wen_comp : STD_LOGIC;
  SIGNAL acc_rsci_wen_comp : STD_LOGIC;
  SIGNAL acc_rsci_idat_mxwt : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_z_oreg : STD_LOGIC_VECTOR (43 DOWNTO 0);
  SIGNAL x1_rsci_idat_10_3 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL x1_rsci_idat_2 : STD_LOGIC;
  SIGNAL x1_rsci_idat_1 : STD_LOGIC;
  SIGNAL x1_rsci_idat_0 : STD_LOGIC;
  SIGNAL y1_rsci_idat_9_4 : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL y1_rsci_idat_3 : STD_LOGIC;
  SIGNAL y1_rsci_idat_2 : STD_LOGIC;
  SIGNAL y1_rsci_idat_1 : STD_LOGIC;
  SIGNAL y1_rsci_idat_0 : STD_LOGIC;
  SIGNAL x2_rsci_idat_10_3 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL x2_rsci_idat_2 : STD_LOGIC;
  SIGNAL x2_rsci_idat_1 : STD_LOGIC;
  SIGNAL x2_rsci_idat_0 : STD_LOGIC;
  SIGNAL y2_rsci_idat_9_4 : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL y2_rsci_idat_3 : STD_LOGIC;
  SIGNAL y2_rsci_idat_2 : STD_LOGIC;
  SIGNAL y2_rsci_idat_1 : STD_LOGIC;
  SIGNAL y2_rsci_idat_0 : STD_LOGIC;
  SIGNAL fsm_output : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL or_dcpl_4 : STD_LOGIC;
  SIGNAL or_dcpl_13 : STD_LOGIC;
  SIGNAL or_dcpl_69 : STD_LOGIC;
  SIGNAL or_dcpl_72 : STD_LOGIC;
  SIGNAL or_dcpl_75 : STD_LOGIC;
  SIGNAL or_dcpl_77 : STD_LOGIC;
  SIGNAL or_dcpl_81 : STD_LOGIC;
  SIGNAL or_dcpl_82 : STD_LOGIC;
  SIGNAL or_dcpl_83 : STD_LOGIC;
  SIGNAL or_dcpl_94 : STD_LOGIC;
  SIGNAL and_dcpl_93 : STD_LOGIC;
  SIGNAL not_tmp_80 : STD_LOGIC;
  SIGNAL not_tmp_82 : STD_LOGIC;
  SIGNAL or_dcpl_103 : STD_LOGIC;
  SIGNAL or_dcpl_109 : STD_LOGIC;
  SIGNAL or_dcpl_110 : STD_LOGIC;
  SIGNAL or_dcpl_131 : STD_LOGIC;
  SIGNAL or_dcpl_141 : STD_LOGIC;
  SIGNAL and_dcpl_103 : STD_LOGIC;
  SIGNAL and_dcpl_105 : STD_LOGIC;
  SIGNAL or_dcpl_149 : STD_LOGIC;
  SIGNAL or_tmp_79 : STD_LOGIC;
  SIGNAL or_tmp_209 : STD_LOGIC;
  SIGNAL or_tmp_581 : STD_LOGIC;
  SIGNAL or_tmp_695 : STD_LOGIC;
  SIGNAL and_444_cse : STD_LOGIC;
  SIGNAL operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
      : STD_LOGIC;
  SIGNAL T_LINE_slc_T_LINE_acc_6_itm : STD_LOGIC;
  SIGNAL T_LINE_if_if_slc_T_LINE_if_acc_8_svs : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL operator_27_3_true_AC_TRN_AC_WRAP_return_sva : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_else_dividend1_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend2_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_else_dividend2_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
      : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
      : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 : STD_LOGIC;
  SIGNAL x2_t_and_1_cse : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_and_cse : STD_LOGIC;
  SIGNAL reg_acc_rsci_irdy_core_psct_cse : STD_LOGIC;
  SIGNAL reg_y2_rsci_ivld_core_psct_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL T_LINE_asn_48 : STD_LOGIC;
  SIGNAL T_LINE_asn_46 : STD_LOGIC;
  SIGNAL R_LINE_r_10_0_sva : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
      : STD_LOGIC;
  SIGNAL ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0
      : STD_LOGIC;
  SIGNAL mux_19_itm : STD_LOGIC;
  SIGNAL mux_20_itm : STD_LOGIC;
  SIGNAL z_out : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL z_out_1 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL z_out_2 : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL z_out_3 : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL or_tmp_777 : STD_LOGIC;
  SIGNAL or_tmp_778 : STD_LOGIC;
  SIGNAL or_tmp_779 : STD_LOGIC;
  SIGNAL or_tmp_780 : STD_LOGIC;
  SIGNAL z_out_4 : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL or_tmp_789 : STD_LOGIC;
  SIGNAL or_tmp_790 : STD_LOGIC;
  SIGNAL z_out_5 : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL or_tmp_792 : STD_LOGIC;
  SIGNAL or_tmp_793 : STD_LOGIC;
  SIGNAL z_out_6 : STD_LOGIC_VECTOR (43 DOWNTO 0);
  SIGNAL data_out_out : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL z_out_7 : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL or_tmp_800 : STD_LOGIC;
  SIGNAL or_tmp_802 : STD_LOGIC;
  SIGNAL z_out_8 : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL threshold_23_8_lpi_3 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL x1_t_26_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_27_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_25_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_28_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_24_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_29_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_23_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_30_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_42_31_lpi_3 : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL y1_t_27_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_26_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_28_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_25_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_29_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_24_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_30_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_23_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_31_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_42_32_lpi_3 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL x2_t_26_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_27_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_25_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_28_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_24_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_29_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_23_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_30_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_42_31_lpi_3 : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL y2_t_27_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_26_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_28_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_25_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_29_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_24_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_30_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_23_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_31_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_42_32_lpi_3 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL T_LINE_t_7_0_sva : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_return_2_69_38_sva : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_a_mx0c0 : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_a_mx0c3 : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL T_LINE_if_acc_5_psp_1 : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (27 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL exs_tmp_16_26_0 : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      : STD_LOGIC;
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
      : STD_LOGIC_VECTOR (25 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c
      : STD_LOGIC;
  SIGNAL and_438_rgt : STD_LOGIC;
  SIGNAL and_499_rgt : STD_LOGIC;
  SIGNAL and_501_rgt : STD_LOGIC;
  SIGNAL and_503_rgt : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      : STD_LOGIC;
  SIGNAL T_LINE_if_or_ssc : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      : STD_LOGIC;
  SIGNAL nor_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL x1_t_and_3_cse : STD_LOGIC;
  SIGNAL x2_t_and_4_cse : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse
      : STD_LOGIC;
  SIGNAL x2_t_and_3_cse : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse
      : STD_LOGIC;
  SIGNAL T_LINE_if_and_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_70_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_60_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_2_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      : STD_LOGIC;
  SIGNAL T_LINE_if_mux1h_4_rgt : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL T_LINE_if_mux1h_8_rgt : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL T_LINE_if_acc_2_itm_16_14 : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL T_LINE_if_acc_2_itm_13 : STD_LOGIC;
  SIGNAL T_LINE_if_acc_2_itm_12_4 : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL T_LINE_if_acc_1_itm_12_4 : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL T_LINE_if_acc_1_itm_3_0 : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL and_1762_cse : STD_LOGIC;
  SIGNAL nor_66_cse : STD_LOGIC;
  SIGNAL and_1771_itm : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_or_itm : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_or_1_itm : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse
      : STD_LOGIC;
  SIGNAL nor_55_cse : STD_LOGIC;
  SIGNAL or_948_cse_1 : STD_LOGIC;

  SIGNAL T_LINE_if_T_LINE_if_and_34_nl : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL T_LINE_if_aelse_not_57_nl : STD_LOGIC;
  SIGNAL mux_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl
      : STD_LOGIC;
  SIGNAL T_LINE_if_T_LINE_if_and_35_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL mux_22_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_19_nl
      : STD_LOGIC;
  SIGNAL T_LINE_if_acc_nl : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl : STD_LOGIC;
  SIGNAL T_LINE_if_aelse_acc_nl : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL or_365_nl : STD_LOGIC;
  SIGNAL operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl : STD_LOGIC_VECTOR (8 DOWNTO
      0);
  SIGNAL or_367_nl : STD_LOGIC;
  SIGNAL or_368_nl : STD_LOGIC;
  SIGNAL mux_23_nl : STD_LOGIC;
  SIGNAL or_nl : STD_LOGIC;
  SIGNAL or_1032_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_11_nl
      : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_24_nl : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_23_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_8_nl
      : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_19_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_6_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_5_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_4_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_3_nl
      : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_13_nl : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_12_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_2_nl
      : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
      : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_7_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
      : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_not_5_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_104_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_105_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_106_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_107_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_108_nl
      : STD_LOGIC;
  SIGNAL or_709_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_not_4_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_nand_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
      : STD_LOGIC;
  SIGNAL not_586_nl : STD_LOGIC;
  SIGNAL mux_24_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
      : STD_LOGIC;
  SIGNAL T_LINE_if_aelse_not_74_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl
      : STD_LOGIC_VECTOR (27 DOWNTO 0);
  SIGNAL or_170_nl : STD_LOGIC;
  SIGNAL or_172_nl : STD_LOGIC;
  SIGNAL T_LINE_if_T_LINE_if_T_LINE_if_nor_2_nl : STD_LOGIC;
  SIGNAL T_LINE_if_mux1h_25_nl : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL T_LINE_if_T_LINE_if_or_1_nl : STD_LOGIC;
  SIGNAL T_LINE_if_T_LINE_if_T_LINE_if_nor_3_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL T_LINE_if_or_2_nl : STD_LOGIC;
  SIGNAL T_LINE_mux1h_4_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux1h_11_nl
      : STD_LOGIC_VECTOR (25 DOWNTO 0);
  SIGNAL or_1063_nl : STD_LOGIC;
  SIGNAL or_1064_nl : STD_LOGIC;
  SIGNAL acc_3_nl : STD_LOGIC_VECTOR (36 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_5_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_6_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl
      : STD_LOGIC_VECTOR (17 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl
      : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_4_nl : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux_1_nl
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_4_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_5_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_4_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL acc_5_nl : STD_LOGIC_VECTOR (44 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_5_nl
      : STD_LOGIC_VECTOR (43 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_6_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_6_nl
      : STD_LOGIC_VECTOR (39 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_23_nl
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL acc_7_nl : STD_LOGIC_VECTOR (17 DOWNTO 0);
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl : STD_LOGIC_VECTOR (16 DOWNTO
      0);
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl : STD_LOGIC_VECTOR (15 DOWNTO
      0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_27_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_28_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_30_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_31_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_27_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_28_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_30_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_31_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_27_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_28_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_30_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_27_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_28_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_30_nl
      : STD_LOGIC;
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a : STD_LOGIC_VECTOR (32 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s : STD_LOGIC_VECTOR (4 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z : STD_LOGIC_VECTOR (32 DOWNTO
      0);

  SIGNAL ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
      : STD_LOGIC_VECTOR (31 DOWNTO 0);

  COMPONENT getMaxLine_core_x1_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
      x1_rsc_vld : OUT STD_LOGIC;
      x1_rsc_rdy : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      x1_rsci_oswt : IN STD_LOGIC;
      x1_rsci_wen_comp : OUT STD_LOGIC;
      x1_rsci_idat : IN STD_LOGIC_VECTOR (10 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_x1_rsci_inst_x1_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_core_x1_rsci_inst_x1_rsci_idat : STD_LOGIC_VECTOR (10 DOWNTO
      0);

  COMPONENT getMaxLine_core_y1_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
      y1_rsc_vld : OUT STD_LOGIC;
      y1_rsc_rdy : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      y1_rsci_oswt : IN STD_LOGIC;
      y1_rsci_wen_comp : OUT STD_LOGIC;
      y1_rsci_idat : IN STD_LOGIC_VECTOR (9 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_y1_rsci_inst_y1_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_core_y1_rsci_inst_y1_rsci_idat : STD_LOGIC_VECTOR (9 DOWNTO 0);

  COMPONENT getMaxLine_core_x2_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
      x2_rsc_vld : OUT STD_LOGIC;
      x2_rsc_rdy : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      x2_rsci_oswt : IN STD_LOGIC;
      x2_rsci_wen_comp : OUT STD_LOGIC;
      x2_rsci_idat : IN STD_LOGIC_VECTOR (10 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_x2_rsci_inst_x2_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_core_x2_rsci_inst_x2_rsci_idat : STD_LOGIC_VECTOR (10 DOWNTO
      0);

  COMPONENT getMaxLine_core_y2_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
      y2_rsc_vld : OUT STD_LOGIC;
      y2_rsc_rdy : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      y2_rsci_oswt : IN STD_LOGIC;
      y2_rsci_wen_comp : OUT STD_LOGIC;
      y2_rsci_idat : IN STD_LOGIC_VECTOR (9 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_y2_rsci_inst_y2_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_core_y2_rsci_inst_y2_rsci_idat : STD_LOGIC_VECTOR (9 DOWNTO 0);

  COMPONENT getMaxLine_core_acc_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      acc_rsc_dat : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsc_vld : IN STD_LOGIC;
      acc_rsc_rdy : OUT STD_LOGIC;
      core_wen : IN STD_LOGIC;
      acc_rsci_oswt : IN STD_LOGIC;
      acc_rsci_wen_comp : OUT STD_LOGIC;
      acc_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_acc_rsci_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO
      0);
  SIGNAL getMaxLine_core_acc_rsci_inst_acc_rsci_idat_mxwt : STD_LOGIC_VECTOR (15
      DOWNTO 0);

  COMPONENT getMaxLine_core_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0);
      core_wen : IN STD_LOGIC;
      T_LINE_if_if_dividend1_mul_cmp_z_oreg : OUT STD_LOGIC_VECTOR (43 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z : STD_LOGIC_VECTOR
      (43 DOWNTO 0);
  SIGNAL getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z_oreg : STD_LOGIC_VECTOR
      (43 DOWNTO 0);

  COMPONENT getMaxLine_core_staller
    PORT(
      core_wen : OUT STD_LOGIC;
      x1_rsci_wen_comp : IN STD_LOGIC;
      y1_rsci_wen_comp : IN STD_LOGIC;
      x2_rsci_wen_comp : IN STD_LOGIC;
      y2_rsci_wen_comp : IN STD_LOGIC;
      acc_rsci_wen_comp : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT getMaxLine_core_core_fsm
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      fsm_output : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
      T_LINE_C_4_tr0 : IN STD_LOGIC;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
          : IN STD_LOGIC;
      T_LINE_C_10_tr0 : IN STD_LOGIC;
      T_LINE_C_10_tr1 : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
          : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
          : IN STD_LOGIC;
      T_LINE_C_14_tr0 : IN STD_LOGIC;
      T_LINE_C_14_tr1 : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
          : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
          : IN STD_LOGIC;
      T_LINE_C_18_tr0 : IN STD_LOGIC;
      R_LINE_C_0_tr0 : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_core_fsm_inst_fsm_output : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_4_tr0 : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0 : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0 : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0 : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0 : STD_LOGIC;

  FUNCTION CONV_SL_1_1(input_val:BOOLEAN)
  RETURN STD_LOGIC IS
  BEGIN
    IF input_val THEN RETURN '1';ELSE RETURN '0';END IF;
  END;

  FUNCTION MUX1HOT_s_1_3_2(input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_s_1_4_2(input_3 : STD_LOGIC;
  input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
      tmp := sel(3);
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_s_1_5_2(input_4 : STD_LOGIC;
  input_3 : STD_LOGIC;
  input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(4 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
      tmp := sel(3);
      result := result or ( input_3 and tmp);
      tmp := sel(4);
      result := result or ( input_4 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_s_1_6_2(input_5 : STD_LOGIC;
  input_4 : STD_LOGIC;
  input_3 : STD_LOGIC;
  input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(5 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
      tmp := sel(3);
      result := result or ( input_3 and tmp);
      tmp := sel(4);
      result := result or ( input_4 and tmp);
      tmp := sel(5);
      result := result or ( input_5 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_11_3_2(input_2 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(10 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(10 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_13_3_2(input_2 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(12 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(12 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_16_13_2(input_12 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_11 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_10 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_9 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_8 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_7 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(12 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
      tmp := (OTHERS=>sel( 8));
      result := result or ( input_8 and tmp);
      tmp := (OTHERS=>sel( 9));
      result := result or ( input_9 and tmp);
      tmp := (OTHERS=>sel( 10));
      result := result or ( input_10 and tmp);
      tmp := (OTHERS=>sel( 11));
      result := result or ( input_11 and tmp);
      tmp := (OTHERS=>sel( 12));
      result := result or ( input_12 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_16_8_2(input_7 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(7 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_17_9_2(input_8 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_7 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(8 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(16 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(16 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
      tmp := (OTHERS=>sel( 8));
      result := result or ( input_8 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_18_9_2(input_8 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_7 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(8 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(17 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(17 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
      tmp := (OTHERS=>sel( 8));
      result := result or ( input_8 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_19_6_2(input_5 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(5 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(18 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(18 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_26_3_2(input_2 : STD_LOGIC_VECTOR(25 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(25 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(25 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(25 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(25 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_2_4_2(input_3 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(1 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(1 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_33_3_2(input_2 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_33_4_2(input_3 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_35_7_2(input_6 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(6 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(34 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(34 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_3_4_2(input_3 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(2 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(2 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_40_4_2(input_3 : STD_LOGIC_VECTOR(39 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(39 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(39 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(39 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(39 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(39 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_44_4_2(input_3 : STD_LOGIC_VECTOR(43 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(43 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(43 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(43 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(43 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(43 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX_s_1_2_2(input_0 : STD_LOGIC;
  input_1 : STD_LOGIC;
  sel : STD_LOGIC)
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_11_2_2(input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(10 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_12_2_2(input_0 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(11 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_16_2_2(input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_20_2_2(input_0 : STD_LOGIC_VECTOR(19 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(19 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(19 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_27_2_2(input_0 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(26 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_28_2_2(input_0 : STD_LOGIC_VECTOR(27 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(27 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(27 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_2_2_2(input_0 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(1 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_32_2_2(input_0 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(31 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_33_2_2(input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_35_2_2(input_0 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(34 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_3_2_2(input_0 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(2 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_5_2_2(input_0 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(4 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_6_2_2(input_0 : STD_LOGIC_VECTOR(5 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(5 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(5 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_8_2_2(input_0 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(7 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

BEGIN
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg : work.mgc_shift_comps_v5.mgc_shift_r_v5
    GENERIC MAP(
      width_a => 33,
      signd_a => 1,
      width_s => 5,
      width_z => 33
      )
    PORT MAP(
      a => operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a,
      s => operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s,
      z => operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z
    );
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a <= (MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18,
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11,
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3,
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1,
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8)) & (fsm_output(6)) & (fsm_output(9)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0,
      T_LINE_slc_T_LINE_acc_6_itm, STD_LOGIC_VECTOR'( (fsm_output(7)) & (fsm_output(8))
      & (fsm_output(6)) & (fsm_output(9)))));
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      (fsm_output(6)) OR (fsm_output(9)));
  z_out_2 <= operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z;

  ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg : work.hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60_pkg.hough_algorithm_hw_1296_864mgc_rom_23_70_32_1_60
    PORT MAP(
      addr => ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr,
      data_out => ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
    );
  ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr <=
      STD_LOGIC_VECTOR(UNSIGNED'( "00") & UNSIGNED(MUX_v_5_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      fsm_output(7))));
  data_out_out <= ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out;

  getMaxLine_core_x1_rsci_inst : getMaxLine_core_x1_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      x1_rsc_dat => getMaxLine_core_x1_rsci_inst_x1_rsc_dat,
      x1_rsc_vld => x1_rsc_vld,
      x1_rsc_rdy => x1_rsc_rdy,
      core_wen => core_wen,
      x1_rsci_oswt => reg_y2_rsci_ivld_core_psct_cse,
      x1_rsci_wen_comp => x1_rsci_wen_comp,
      x1_rsci_idat => getMaxLine_core_x1_rsci_inst_x1_rsci_idat
    );
  x1_rsc_dat <= getMaxLine_core_x1_rsci_inst_x1_rsc_dat;
  getMaxLine_core_x1_rsci_inst_x1_rsci_idat <= x1_rsci_idat_10_3 & x1_rsci_idat_2
      & x1_rsci_idat_1 & x1_rsci_idat_0;

  getMaxLine_core_y1_rsci_inst : getMaxLine_core_y1_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      y1_rsc_dat => getMaxLine_core_y1_rsci_inst_y1_rsc_dat,
      y1_rsc_vld => y1_rsc_vld,
      y1_rsc_rdy => y1_rsc_rdy,
      core_wen => core_wen,
      y1_rsci_oswt => reg_y2_rsci_ivld_core_psct_cse,
      y1_rsci_wen_comp => y1_rsci_wen_comp,
      y1_rsci_idat => getMaxLine_core_y1_rsci_inst_y1_rsci_idat
    );
  y1_rsc_dat <= getMaxLine_core_y1_rsci_inst_y1_rsc_dat;
  getMaxLine_core_y1_rsci_inst_y1_rsci_idat <= y1_rsci_idat_9_4 & y1_rsci_idat_3
      & y1_rsci_idat_2 & y1_rsci_idat_1 & y1_rsci_idat_0;

  getMaxLine_core_x2_rsci_inst : getMaxLine_core_x2_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      x2_rsc_dat => getMaxLine_core_x2_rsci_inst_x2_rsc_dat,
      x2_rsc_vld => x2_rsc_vld,
      x2_rsc_rdy => x2_rsc_rdy,
      core_wen => core_wen,
      x2_rsci_oswt => reg_y2_rsci_ivld_core_psct_cse,
      x2_rsci_wen_comp => x2_rsci_wen_comp,
      x2_rsci_idat => getMaxLine_core_x2_rsci_inst_x2_rsci_idat
    );
  x2_rsc_dat <= getMaxLine_core_x2_rsci_inst_x2_rsc_dat;
  getMaxLine_core_x2_rsci_inst_x2_rsci_idat <= x2_rsci_idat_10_3 & x2_rsci_idat_2
      & x2_rsci_idat_1 & x2_rsci_idat_0;

  getMaxLine_core_y2_rsci_inst : getMaxLine_core_y2_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      y2_rsc_dat => getMaxLine_core_y2_rsci_inst_y2_rsc_dat,
      y2_rsc_vld => y2_rsc_vld,
      y2_rsc_rdy => y2_rsc_rdy,
      core_wen => core_wen,
      y2_rsci_oswt => reg_y2_rsci_ivld_core_psct_cse,
      y2_rsci_wen_comp => y2_rsci_wen_comp,
      y2_rsci_idat => getMaxLine_core_y2_rsci_inst_y2_rsci_idat
    );
  y2_rsc_dat <= getMaxLine_core_y2_rsci_inst_y2_rsc_dat;
  getMaxLine_core_y2_rsci_inst_y2_rsci_idat <= y2_rsci_idat_9_4 & y2_rsci_idat_3
      & y2_rsci_idat_2 & y2_rsci_idat_1 & y2_rsci_idat_0;

  getMaxLine_core_acc_rsci_inst : getMaxLine_core_acc_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      acc_rsc_dat => getMaxLine_core_acc_rsci_inst_acc_rsc_dat,
      acc_rsc_vld => acc_rsc_vld,
      acc_rsc_rdy => acc_rsc_rdy,
      core_wen => core_wen,
      acc_rsci_oswt => reg_acc_rsci_irdy_core_psct_cse,
      acc_rsci_wen_comp => acc_rsci_wen_comp,
      acc_rsci_idat_mxwt => getMaxLine_core_acc_rsci_inst_acc_rsci_idat_mxwt
    );
  getMaxLine_core_acc_rsci_inst_acc_rsc_dat <= acc_rsc_dat;
  acc_rsci_idat_mxwt <= getMaxLine_core_acc_rsci_inst_acc_rsci_idat_mxwt;

  getMaxLine_core_wait_dp_inst : getMaxLine_core_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      T_LINE_if_if_dividend1_mul_cmp_z => getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z,
      core_wen => core_wen,
      T_LINE_if_if_dividend1_mul_cmp_z_oreg => getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z_oreg
    );
  getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z <= T_LINE_if_if_dividend1_mul_cmp_z;
  T_LINE_if_if_dividend1_mul_cmp_z_oreg <= getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z_oreg;

  getMaxLine_core_staller_inst : getMaxLine_core_staller
    PORT MAP(
      core_wen => core_wen,
      x1_rsci_wen_comp => x1_rsci_wen_comp,
      y1_rsci_wen_comp => y1_rsci_wen_comp,
      x2_rsci_wen_comp => x2_rsci_wen_comp,
      y2_rsci_wen_comp => y2_rsci_wen_comp,
      acc_rsci_wen_comp => acc_rsci_wen_comp
    );
  getMaxLine_core_core_fsm_inst : getMaxLine_core_core_fsm
    PORT MAP(
      clk => clk,
      rst => rst,
      core_wen => core_wen,
      fsm_output => getMaxLine_core_core_fsm_inst_fsm_output,
      T_LINE_C_4_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_4_tr0,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
          => ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm,
      T_LINE_C_10_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0,
      T_LINE_C_10_tr1 => T_LINE_asn_46,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_14_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0,
      T_LINE_C_14_tr1 => T_LINE_asn_46,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_18_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0,
      R_LINE_C_0_tr0 => getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0
    );
  fsm_output <= getMaxLine_core_core_fsm_inst_fsm_output;
  getMaxLine_core_core_fsm_inst_T_LINE_C_4_tr0 <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0 <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      <= NOT (z_out_6(4));
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      <= NOT (z_out_6(4));
  getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0 <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      <= NOT (z_out_6(4));
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      <= NOT (z_out_6(4));
  getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0 <= NOT T_LINE_slc_T_LINE_acc_6_itm;
  getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0 <= NOT (z_out_8(8));

  nor_cse <= NOT((fsm_output(30)) OR (fsm_output(0)));
  x1_t_and_3_cse <= core_wen AND or_tmp_79;
  x2_t_and_1_cse <= T_LINE_asn_48 AND (fsm_output(29));
  x2_t_and_3_cse <= core_wen AND (T_LINE_asn_46 OR T_LINE_asn_48) AND (fsm_output(29));
  x2_t_and_4_cse <= core_wen AND (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      AND (fsm_output(29));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      <= core_wen AND (fsm_output(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      <= core_wen AND (NOT or_dcpl_4) AND (fsm_output(22));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse
      <= core_wen AND (NOT or_dcpl_4) AND (fsm_output(28));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      <= core_wen AND (fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      <= core_wen AND (NOT or_dcpl_13) AND (fsm_output(22));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse
      <= core_wen AND (NOT or_dcpl_13) AND (fsm_output(28));
  Hough_Algorithm_HW_1296_864_getMaxLine_and_cse <= core_wen AND (NOT((NOT (fsm_output(30)))
      OR (z_out_8(8))));
  and_438_rgt <= (NOT (z_out_8(16))) AND (fsm_output(1));
  T_LINE_if_if_dividend1_mul_cmp_a <= reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd &
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9;
  and_499_rgt <= (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      AND (fsm_output(4));
  and_501_rgt <= (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      AND (fsm_output(12));
  and_503_rgt <= (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      AND (fsm_output(13));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      <= MUX_v_6_2_2(STD_LOGIC_VECTOR'("000000"), (z_out_1(5 DOWNTO 0)), or_tmp_695);
  or_365_nl <= or_dcpl_72 OR or_dcpl_69 OR (fsm_output(19));
  T_LINE_if_mux1h_4_rgt <= MUX1HOT_v_13_3_2((z_out(12 DOWNTO 0)), (STD_LOGIC_VECTOR'(
      "000") & (T_LINE_if_acc_5_psp_1(9 DOWNTO 8)) & STD_LOGIC_VECTOR'( "00000000")),
      (STD_LOGIC_VECTOR'( "0000000") & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl),
      STD_LOGIC_VECTOR'( (fsm_output(2)) & (fsm_output(4)) & or_365_nl));
  operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(R_LINE_r_10_0_sva(10
      DOWNTO 2)) + UNSIGNED'( "100011011"), 9));
  or_367_nl <= (fsm_output(11)) OR (fsm_output(4)) OR or_dcpl_77 OR or_dcpl_75 OR
      (fsm_output(5)) OR (fsm_output(7));
  or_368_nl <= (fsm_output(16)) OR (fsm_output(20)) OR or_dcpl_83 OR or_dcpl_82;
  T_LINE_if_mux1h_8_rgt <= MUX1HOT_v_13_3_2((z_out_8(12 DOWNTO 0)), (STD_LOGIC_VECTOR'(
      "000") & T_LINE_if_acc_5_psp_1), (STD_LOGIC_VECTOR'( "0000") & STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl),
      9))), STD_LOGIC_VECTOR'( (fsm_output(3)) & or_367_nl & or_368_nl));
  T_LINE_if_and_cse <= core_wen AND CONV_SL_1_1(fsm_output(11 DOWNTO 5)=STD_LOGIC_VECTOR'("0000000"));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_70_cse
      <= core_wen AND (NOT or_tmp_209);
  and_1762_cse <= ((fsm_output(11)) OR (fsm_output(5))) AND core_wen;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_60_cse
      <= core_wen AND (NOT((fsm_output(9)) OR (fsm_output(6)) OR or_dcpl_94));
  nor_55_cse <= NOT(CONV_SL_1_1(fsm_output(7 DOWNTO 6)/=STD_LOGIC_VECTOR'("00")));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      AND (fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      <= (NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs)
      AND (fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt
      <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      OR ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_2_cse <=
      core_wen AND (NOT(or_dcpl_77 OR (fsm_output(6)) OR or_dcpl_94));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt <=
      ((NOT mux_19_itm) AND (fsm_output(22))) OR ((NOT mux_20_itm) AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt <=
      (mux_19_itm AND (fsm_output(22))) OR (mux_20_itm AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt <=
      (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
      AND (fsm_output(22))) OR (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
      AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt <=
      (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs)
      AND (fsm_output(22))) OR (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs)
      AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt <=
      (not_tmp_80 AND (fsm_output(22))) OR (not_tmp_82 AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt <=
      ((NOT(T_LINE_if_if_slc_T_LINE_if_acc_8_svs OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs))
      AND (fsm_output(22))) OR ((NOT(T_LINE_if_if_slc_T_LINE_if_acc_8_svs OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs))
      AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_not_4_nl
      <= NOT and_dcpl_93;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl
      <= MUX_v_32_2_2(STD_LOGIC_VECTOR'("00000000000000000000000000000000"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm(31
      DOWNTO 0)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_not_4_nl);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt
      <= MUX_v_33_2_2(('0' & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl),
      z_out_2, fsm_output(8));
  nor_66_cse <= NOT(CONV_SL_1_1(fsm_output(10 DOWNTO 9)/=STD_LOGIC_VECTOR'("00")));
  and_1771_itm <= nor_66_cse AND core_wen;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_nand_nl
      <= NOT(and_dcpl_93 AND (NOT (fsm_output(7))));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt
      <= MUX_v_35_2_2((STD_LOGIC_VECTOR'( "000") & STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_nand_nl,
      1),32))), (z_out_4(34 DOWNTO 0)), fsm_output(8));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
      <= core_wen AND (NOT(or_dcpl_77 OR or_dcpl_94));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt
      <= (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
      OR ((NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      <= (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp)
      AND (fsm_output(9));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
      AND (fsm_output(9));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      <= ((NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32)))
      OR (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse
      <= core_wen AND (NOT(or_dcpl_149 OR (fsm_output(16)) OR (fsm_output(23)) OR
      (fsm_output(24)) OR (fsm_output(15)) OR (fsm_output(17)) OR (fsm_output(18))
      OR or_dcpl_81));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c
      <= (fsm_output(18)) OR (fsm_output(24));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c
      <= (fsm_output(19)) OR (fsm_output(25));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse
      <= ((NOT(T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (z_out_6(4)))) AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
      OR ((T_LINE_if_if_slc_T_LINE_if_acc_8_svs OR (NOT (z_out_6(4)))) AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c)
      OR or_dcpl_131 OR CONV_SL_1_1(fsm_output(27 DOWNTO 26)/=STD_LOGIC_VECTOR'("00"));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      <= (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND (fsm_output(17));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (fsm_output(17));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      <= (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND (fsm_output(23));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (fsm_output(23));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0
      <= NOT(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
      AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  T_LINE_if_acc_5_psp_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED(T_LINE_if_acc_2_itm_16_14
      & T_LINE_if_acc_2_itm_13 & (T_LINE_if_acc_2_itm_12_4(8 DOWNTO 4))), 9), 10)
      + UNSIGNED((T_LINE_t_7_0_sva(6 DOWNTO 0)) & STD_LOGIC_VECTOR'( "111")), 10));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2
      <= (T_LINE_if_acc_1_itm_12_4(4)) XOR (T_LINE_if_acc_1_itm_12_4(5));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2,
      1),2)), STD_LOGIC_VECTOR'( "01"), T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1
      <= NOT(MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
      1),3)), STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2));
  T_LINE_if_aelse_not_74_nl <= NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_aelse_not_74_nl,
      1),2)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_if_dividend1_sva(18 DOWNTO 0))), z_out, T_LINE_if_if_dividend1_sva(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_else_dividend1_sva(18 DOWNTO 0))), z_out,
      T_LINE_if_else_dividend1_sva(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl
      <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0)
      + CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1),
      27), 28), 28));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0
      <= MUX_v_28_2_2(('0' & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl),
      28)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1
      <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(CONV_UNSIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm,
      1), 1), 20) + SIGNED'( (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0)),
      20));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_if_dividend2_sva(18 DOWNTO 0))), z_out, T_LINE_if_if_dividend2_sva(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_else_dividend2_sva(18 DOWNTO 0))), z_out,
      T_LINE_if_else_dividend2_sva(19));
  T_LINE_asn_46 <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  T_LINE_asn_48 <= NOT(T_LINE_if_if_slc_T_LINE_if_acc_8_svs OR operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  or_dcpl_4 <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  or_dcpl_13 <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      OR (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  or_dcpl_69 <= CONV_SL_1_1(fsm_output(18 DOWNTO 17)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_72 <= CONV_SL_1_1(fsm_output(25 DOWNTO 23)/=STD_LOGIC_VECTOR'("000"));
  or_dcpl_75 <= (fsm_output(6)) OR (fsm_output(8));
  or_dcpl_77 <= CONV_SL_1_1(fsm_output(10 DOWNTO 9)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_81 <= (fsm_output(19)) OR (fsm_output(21));
  or_dcpl_82 <= or_dcpl_69 OR or_dcpl_81;
  or_dcpl_83 <= CONV_SL_1_1(fsm_output(15 DOWNTO 14)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_94 <= CONV_SL_1_1(fsm_output(8 DOWNTO 7)/=STD_LOGIC_VECTOR'("00"));
  and_dcpl_93 <= NOT((fsm_output(11)) OR (fsm_output(6)));
  not_tmp_80 <= NOT((NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs)
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  or_170_nl <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  mux_19_itm <= MUX_s_1_2_2(not_tmp_80, or_170_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  not_tmp_82 <= NOT((NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs)
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  or_172_nl <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  mux_20_itm <= MUX_s_1_2_2(not_tmp_82, or_172_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  or_dcpl_103 <= (fsm_output(12)) OR (fsm_output(8));
  or_dcpl_109 <= (fsm_output(16)) OR (fsm_output(23));
  or_dcpl_110 <= (fsm_output(11)) OR (fsm_output(22));
  or_dcpl_131 <= CONV_SL_1_1(fsm_output(21 DOWNTO 20)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_141 <= CONV_SL_1_1(fsm_output(19 DOWNTO 18)/=STD_LOGIC_VECTOR'("00"));
  and_dcpl_103 <= NOT(T_LINE_if_if_slc_T_LINE_if_acc_8_svs OR (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32)));
  and_dcpl_105 <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32)));
  or_dcpl_149 <= (fsm_output(22)) OR (fsm_output(25));
  or_tmp_79 <= (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      AND (fsm_output(26));
  and_444_cse <= (NOT (z_out_8(8))) AND (fsm_output(30));
  or_tmp_209 <= or_dcpl_75 OR (fsm_output(7));
  or_tmp_581 <= (fsm_output(23)) OR (fsm_output(17));
  or_tmp_695 <= CONV_SL_1_1(fsm_output(25 DOWNTO 24)/=STD_LOGIC_VECTOR'("00")) OR
      or_dcpl_141;
  T_LINE_if_if_dividend1_mul_cmp_a_mx0c0 <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs
      AND (fsm_output(14));
  T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 <= (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs)
      AND (fsm_output(14));
  T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs
      AND (fsm_output(20));
  T_LINE_if_if_dividend1_mul_cmp_a_mx0c3 <= (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs)
      AND (fsm_output(20));
  T_LINE_if_if_dividend1_or_itm <= T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 OR T_LINE_if_if_dividend1_mul_cmp_a_mx0c3;
  T_LINE_if_if_dividend1_or_1_itm <= T_LINE_if_if_dividend1_mul_cmp_a_mx0c0 OR T_LINE_if_if_dividend1_mul_cmp_a_mx0c2;
  or_tmp_777 <= (NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs)
      AND (fsm_output(10));
  or_tmp_778 <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      AND (fsm_output(10));
  or_tmp_779 <= (NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs)
      AND (fsm_output(8));
  or_tmp_780 <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      AND (fsm_output(8));
  or_948_cse_1 <= (fsm_output(25)) OR (fsm_output(24)) OR (fsm_output(19)) OR (fsm_output(18));
  exs_tmp_16_26_0 <= MUX_v_27_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva,
      (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  or_tmp_789 <= (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp)
      AND (fsm_output(7));
  or_tmp_790 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
      AND (fsm_output(7));
  or_tmp_792 <= (fsm_output(21)) OR (fsm_output(15));
  or_tmp_793 <= (fsm_output(22)) OR (fsm_output(16));
  or_tmp_800 <= (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND (fsm_output(29));
  or_tmp_802 <= T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (fsm_output(29));
  T_LINE_if_or_ssc <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_t_7_0_sva <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( (((fsm_output(26)) OR (fsm_output(31)) OR (fsm_output(0)) OR (fsm_output(30)))
          AND core_wen) = '1' ) THEN
        T_LINE_t_7_0_sva <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), (z_out_1(7
            DOWNTO 0)), nor_cse);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x1_t_42_31_lpi_3 <= STD_LOGIC_VECTOR'( "000000000000");
      ELSIF ( (mux_nl AND (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
          AND core_wen) = '1' ) THEN
        x1_t_42_31_lpi_3 <= MUX_v_12_2_2((z_out_8(11 DOWNTO 0)), T_LINE_if_T_LINE_if_and_34_nl,
            or_tmp_79);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x1_t_30_lpi_3 <= '0';
        x1_t_29_lpi_3 <= '0';
        x1_t_28_lpi_3 <= '0';
        x1_t_27_lpi_3 <= '0';
        x1_t_26_lpi_3 <= '0';
        x1_t_25_lpi_3 <= '0';
        x1_t_24_lpi_3 <= '0';
        x1_t_23_lpi_3 <= '0';
        y1_t_31_lpi_3 <= '0';
        y1_t_30_lpi_3 <= '0';
        y1_t_29_lpi_3 <= '0';
        y1_t_28_lpi_3 <= '0';
        y1_t_27_lpi_3 <= '0';
        y1_t_26_lpi_3 <= '0';
        y1_t_25_lpi_3 <= '0';
        y1_t_24_lpi_3 <= '0';
        y1_t_23_lpi_3 <= '0';
      ELSIF ( x1_t_and_3_cse = '1' ) THEN
        x1_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x1_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        y1_t_31_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y1_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_else_dividend1_sva <= STD_LOGIC_VECTOR'( "00000000000000000000");
      ELSIF ( (core_wen AND (NOT or_dcpl_4) AND (fsm_output(16))) = '1' ) THEN
        T_LINE_if_else_dividend1_sva <= z_out_6(43 DOWNTO 24);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x2_t_42_31_lpi_3 <= STD_LOGIC_VECTOR'( "000000000000");
        y2_t_42_32_lpi_3 <= STD_LOGIC_VECTOR'( "00000000000");
      ELSIF ( x2_t_and_3_cse = '1' ) THEN
        x2_t_42_31_lpi_3 <= MUX_v_12_2_2(STD_LOGIC_VECTOR'( "000010100010"), (z_out_8(11
            DOWNTO 0)), x2_t_and_1_cse);
        y2_t_42_32_lpi_3 <= MUX_v_11_2_2((z_out_8(10 DOWNTO 0)), STD_LOGIC_VECTOR'(
            "00000110110"), x2_t_and_1_cse);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x2_t_30_lpi_3 <= '0';
        x2_t_29_lpi_3 <= '0';
        x2_t_28_lpi_3 <= '0';
        x2_t_27_lpi_3 <= '0';
        x2_t_26_lpi_3 <= '0';
        x2_t_25_lpi_3 <= '0';
        x2_t_24_lpi_3 <= '0';
        x2_t_23_lpi_3 <= '0';
        y2_t_31_lpi_3 <= '0';
        y2_t_30_lpi_3 <= '0';
        y2_t_29_lpi_3 <= '0';
        y2_t_28_lpi_3 <= '0';
        y2_t_27_lpi_3 <= '0';
        y2_t_26_lpi_3 <= '0';
        y2_t_25_lpi_3 <= '0';
        y2_t_24_lpi_3 <= '0';
        y2_t_23_lpi_3 <= '0';
      ELSIF ( x2_t_and_4_cse = '1' ) THEN
        x2_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        x2_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl
            AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        y2_t_31_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
        y2_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
            AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
            <= '0';
        operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
            <= NOT((T_LINE_if_else_dividend1_sva(19)) XOR (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32)));
        operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= NOT(CONV_SL_1_1(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
            DOWNTO 6)/=STD_LOGIC_VECTOR'("000000000000000000000000000")));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
            <= '0';
        T_LINE_if_else_dividend2_sva <= STD_LOGIC_VECTOR'( "00000000000000000000");
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
            <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
        T_LINE_if_else_dividend2_sva <= z_out_6(43 DOWNTO 24);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
            <= '0';
      ELSIF ( (core_wen AND (fsm_output(25))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
            <= NOT((T_LINE_if_else_dividend2_sva(19)) XOR (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm
            <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        y1_t_42_32_lpi_3 <= STD_LOGIC_VECTOR'( "00000000000");
      ELSIF ( (mux_22_nl AND (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
          AND core_wen) = '1' ) THEN
        y1_t_42_32_lpi_3 <= MUX_v_11_2_2((z_out_8(10 DOWNTO 0)), T_LINE_if_T_LINE_if_and_35_nl,
            or_tmp_79);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_if_dividend1_sva <= STD_LOGIC_VECTOR'( "00000000000000000000");
      ELSIF ( (core_wen AND (NOT or_dcpl_13) AND (fsm_output(16))) = '1' ) THEN
        T_LINE_if_if_dividend1_sva <= z_out_6(43 DOWNTO 24);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
            <= '0';
        operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
            <= NOT((T_LINE_if_if_dividend1_sva(19)) XOR (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32)));
        operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= NOT(CONV_SL_1_1(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32
            DOWNTO 6)/=STD_LOGIC_VECTOR'("000000000000000000000000000")));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm
            <= '0';
        T_LINE_if_if_dividend2_sva <= STD_LOGIC_VECTOR'( "00000000000000000000");
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
            <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
        T_LINE_if_if_dividend2_sva <= z_out_6(43 DOWNTO 24);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
            <= '0';
      ELSIF ( (core_wen AND (fsm_output(24))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
            <= NOT((T_LINE_if_if_dividend2_sva(19)) XOR (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm
            <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        R_LINE_r_10_0_sva <= STD_LOGIC_VECTOR'( "00000000000");
      ELSIF ( (core_wen AND ((fsm_output(0)) OR (fsm_output(30)))) = '1' ) THEN
        R_LINE_r_10_0_sva <= MUX_v_11_2_2(STD_LOGIC_VECTOR'("00000000000"), z_out_1,
            (fsm_output(30)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        y2_rsci_idat_0 <= '0';
        y2_rsci_idat_1 <= '0';
        y2_rsci_idat_2 <= '0';
        y2_rsci_idat_3 <= '0';
        y2_rsci_idat_9_4 <= STD_LOGIC_VECTOR'( "000000");
        x2_rsci_idat_0 <= '0';
        x2_rsci_idat_1 <= '0';
        x2_rsci_idat_2 <= '0';
        x2_rsci_idat_10_3 <= STD_LOGIC_VECTOR'( "00000000");
        y1_rsci_idat_0 <= '0';
        y1_rsci_idat_1 <= '0';
        y1_rsci_idat_2 <= '0';
        y1_rsci_idat_3 <= '0';
        y1_rsci_idat_9_4 <= STD_LOGIC_VECTOR'( "000000");
        x1_rsci_idat_0 <= '0';
        x1_rsci_idat_1 <= '0';
        x1_rsci_idat_2 <= '0';
        x1_rsci_idat_10_3 <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( Hough_Algorithm_HW_1296_864_getMaxLine_and_cse = '1' ) THEN
        y2_rsci_idat_0 <= y2_t_28_lpi_3;
        y2_rsci_idat_1 <= y2_t_29_lpi_3;
        y2_rsci_idat_2 <= y2_t_30_lpi_3;
        y2_rsci_idat_3 <= y2_t_31_lpi_3;
        y2_rsci_idat_9_4 <= y2_t_42_32_lpi_3(5 DOWNTO 0);
        x2_rsci_idat_0 <= x2_t_28_lpi_3;
        x2_rsci_idat_1 <= x2_t_29_lpi_3;
        x2_rsci_idat_2 <= x2_t_30_lpi_3;
        x2_rsci_idat_10_3 <= x2_t_42_31_lpi_3(7 DOWNTO 0);
        y1_rsci_idat_0 <= y1_t_28_lpi_3;
        y1_rsci_idat_1 <= y1_t_29_lpi_3;
        y1_rsci_idat_2 <= y1_t_30_lpi_3;
        y1_rsci_idat_3 <= y1_t_31_lpi_3;
        y1_rsci_idat_9_4 <= y1_t_42_32_lpi_3(5 DOWNTO 0);
        x1_rsci_idat_0 <= x1_t_28_lpi_3;
        x1_rsci_idat_1 <= x1_t_29_lpi_3;
        x1_rsci_idat_2 <= x1_t_30_lpi_3;
        x1_rsci_idat_10_3 <= x1_t_42_31_lpi_3(7 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        threshold_23_8_lpi_3 <= STD_LOGIC_VECTOR'( "0000000000000000");
      ELSIF ( (core_wen AND ((fsm_output(0)) OR (fsm_output(31)) OR and_438_rgt))
          = '1' ) THEN
        threshold_23_8_lpi_3 <= MUX_v_16_2_2(STD_LOGIC_VECTOR'( "0000000101000100"),
            acc_rsci_idat_mxwt, and_438_rgt);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_acc_rsci_irdy_core_psct_cse <= '0';
        reg_y2_rsci_ivld_core_psct_cse <= '0';
        T_LINE_if_if_dividend1_mul_cmp_b <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
            <= '0';
        T_LINE_slc_T_LINE_acc_6_itm <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
            <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= STD_LOGIC_VECTOR'( "00000000000");
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= '0';
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
            <= '0';
      ELSIF ( core_wen = '1' ) THEN
        reg_acc_rsci_irdy_core_psct_cse <= NOT((nor_cse AND (NOT (fsm_output(29))))
            OR ((NOT T_LINE_slc_T_LINE_acc_6_itm) AND (fsm_output(29))) OR and_444_cse);
        reg_y2_rsci_ivld_core_psct_cse <= and_444_cse;
        T_LINE_if_if_dividend1_mul_cmp_b <= MUX_v_27_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32
            DOWNTO 6)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
            DOWNTO 6)), T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_11_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_10_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_9_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_8_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_7_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_6_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_5_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_4_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_3_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_2_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_1_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_104_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_105_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_106_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_107_nl
            AND (NOT (fsm_output(5)));
        T_LINE_slc_T_LINE_acc_6_itm <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_108_nl
            AND (NOT (fsm_output(5)));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
            <= z_out_8(3);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
            AND (NOT or_tmp_581);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(19)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(19)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(19)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(19)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(18)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(18)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(18)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(18)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(17)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(17)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(17)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(17)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(16)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(16)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(16)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(16)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(15)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(15)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(15)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(15)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(14)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(14)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(14)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(14)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(13)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(13)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(13)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(13)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(12)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(12)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(12)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(12)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(11)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(11)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(11)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(11)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(10)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(10)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(10)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(10)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(9)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(9)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(9)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(9)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(8)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(8)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(8)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(8)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(7)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(7)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(7)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(7)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(6)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(6)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(6)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(6)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(5)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(5)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(5)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(5)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(4)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(4)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(4)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(4)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(3)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(3)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(3)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(3)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(2)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(2)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(2)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(2)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(1)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(1)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(1)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(1)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_695 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
            AND (NOT or_tmp_695);
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= MUX_v_11_2_2((z_out_8(11 DOWNTO
            1)), (z_out_8(10 DOWNTO 0)), T_LINE_if_if_dividend1_or_itm);
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= MUX1HOT_s_1_3_2((z_out_8(0)),
            y1_t_31_lpi_3, y2_t_31_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_or_1_itm
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= MUX1HOT_s_1_4_2(x1_t_30_lpi_3,
            y1_t_30_lpi_3, x2_t_30_lpi_3, y2_t_30_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= MUX1HOT_s_1_4_2(x1_t_29_lpi_3,
            y1_t_29_lpi_3, x2_t_29_lpi_3, y2_t_29_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= MUX1HOT_s_1_4_2(x1_t_28_lpi_3,
            y1_t_28_lpi_3, x2_t_28_lpi_3, y2_t_28_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= MUX1HOT_s_1_4_2(x1_t_27_lpi_3,
            y1_t_27_lpi_3, x2_t_27_lpi_3, y2_t_27_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= MUX1HOT_s_1_4_2(x1_t_26_lpi_3,
            y1_t_26_lpi_3, x2_t_26_lpi_3, y2_t_26_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= MUX1HOT_s_1_4_2(x1_t_25_lpi_3,
            y1_t_25_lpi_3, x2_t_25_lpi_3, y2_t_25_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= MUX1HOT_s_1_4_2(x1_t_24_lpi_3,
            y1_t_24_lpi_3, x2_t_24_lpi_3, y2_t_24_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= MUX1HOT_s_1_4_2(x1_t_23_lpi_3,
            y1_t_23_lpi_3, x2_t_23_lpi_3, y2_t_23_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
            & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(8);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_if_slc_T_LINE_if_acc_8_svs <= '0';
      ELSIF ( (core_wen AND (and_499_rgt OR and_501_rgt OR and_503_rgt)) = '1' )
          THEN
        T_LINE_if_if_slc_T_LINE_if_acc_8_svs <= MUX1HOT_s_1_3_2((z_out(18)), (T_LINE_if_acc_nl(8)),
            T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl, STD_LOGIC_VECTOR'( and_499_rgt
            & and_501_rgt & and_503_rgt));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
            <= '0';
      ELSIF ( (core_wen AND (fsm_output(1))) = '1' ) THEN
        operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
            <= z_out_8(16);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_acc_1_itm_12_4 <= STD_LOGIC_VECTOR'( "000000000");
      ELSIF ( (((fsm_output(18)) OR (fsm_output(19)) OR (fsm_output(24)) OR (fsm_output(25))
          OR (fsm_output(4)) OR (fsm_output(17)) OR (fsm_output(23)) OR (fsm_output(2))
          OR (fsm_output(3))) AND core_wen) = '1' ) THEN
        T_LINE_if_acc_1_itm_12_4 <= T_LINE_if_mux1h_4_rgt(12 DOWNTO 4);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_acc_1_itm_3_0 <= STD_LOGIC_VECTOR'( "0000");
      ELSIF ( (((fsm_output(18)) OR (fsm_output(19)) OR (fsm_output(24)) OR (fsm_output(25))
          OR (fsm_output(4)) OR (fsm_output(17)) OR (fsm_output(23)) OR (fsm_output(2)))
          AND core_wen) = '1' ) THEN
        T_LINE_if_acc_1_itm_3_0 <= T_LINE_if_mux1h_4_rgt(3 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_acc_2_itm_16_14 <= STD_LOGIC_VECTOR'( "000");
      ELSIF ( (((fsm_output(19)) OR (fsm_output(21)) OR (fsm_output(17)) OR (fsm_output(18))
          OR (fsm_output(15)) OR (fsm_output(16)) OR (fsm_output(20)) OR (fsm_output(14))
          OR (fsm_output(3)) OR (fsm_output(4)) OR (fsm_output(7)) OR (fsm_output(5))
          OR (fsm_output(8)) OR (fsm_output(10)) OR (fsm_output(11)) OR (fsm_output(6))
          OR (fsm_output(9))) AND core_wen) = '1' ) THEN
        T_LINE_if_acc_2_itm_16_14 <= T_LINE_if_mux1h_8_rgt(12 DOWNTO 10);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_acc_2_itm_13 <= '0';
      ELSIF ( (((fsm_output(4)) OR (fsm_output(19)) OR (fsm_output(21)) OR (fsm_output(17))
          OR (fsm_output(18)) OR (fsm_output(15)) OR (fsm_output(16)) OR (fsm_output(20))
          OR (fsm_output(14)) OR (fsm_output(3))) AND core_wen) = '1' ) THEN
        T_LINE_if_acc_2_itm_13 <= T_LINE_if_mux1h_8_rgt(9);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_acc_2_itm_12_4 <= STD_LOGIC_VECTOR'( "000000000");
      ELSIF ( ((mux_23_nl OR (fsm_output(3))) AND core_wen) = '1' ) THEN
        T_LINE_if_acc_2_itm_12_4 <= T_LINE_if_mux1h_8_rgt(8 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0
            <= STD_LOGIC_VECTOR'( "0000");
        ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
            <= STD_LOGIC_VECTOR'( "0000");
      ELSIF ( T_LINE_if_and_cse = '1' ) THEN
        ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0
            <= T_LINE_if_acc_1_itm_3_0;
        ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
            <= T_LINE_if_acc_2_itm_12_4(3 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
            <= '0';
      ELSIF ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_70_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1),
            1),3)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32
            DOWNTO 30)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
            <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(29)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(28)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(27)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(26
            DOWNTO 25)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(24)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(23
            DOWNTO 22)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(21)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),3)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(20
            DOWNTO 18)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(17)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
            <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(16)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(15)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(14)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),3)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(13
            DOWNTO 11)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(10)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(9
            DOWNTO 8)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(7)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(6
            DOWNTO 5)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(4)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
            <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(3)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
            <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse,
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(2
            DOWNTO 1)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(29)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(26)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(25)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(23)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(22)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(9)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(8)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(6)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(5)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(3)),
            fsm_output(11));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
            <= MUX1HOT_s_1_4_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(1)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(18)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1,
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(11)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(17)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(8)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(12)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(16)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(17)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(13)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(15)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(16)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(16)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(14)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(15)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(18)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(12)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(13)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(19)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(11)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(12)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
            <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(20)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(19)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(10)),
            STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
            <= STD_LOGIC_VECTOR'( "000");
      ELSIF ( and_1762_cse = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
            DOWNTO 30)), Hough_Algorithm_HW_1296_864_getMaxLine_not_24_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(28
            DOWNTO 27)), Hough_Algorithm_HW_1296_864_getMaxLine_not_23_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(15
            DOWNTO 14)), Hough_Algorithm_HW_1296_864_getMaxLine_not_19_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(32
            DOWNTO 30)), Hough_Algorithm_HW_1296_864_getMaxLine_not_13_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(28
            DOWNTO 27)), Hough_Algorithm_HW_1296_864_getMaxLine_not_12_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(15
            DOWNTO 14)), Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
            <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1,
            Hough_Algorithm_HW_1296_864_getMaxLine_not_7_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
            <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1,
            Hough_Algorithm_HW_1296_864_getMaxLine_not_5_nl);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse),3)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm(34
            DOWNTO 32)), fsm_output(11));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
            <= '0';
      ELSIF ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_60_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(29)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(26)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(25)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(23)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(22)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(20)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(19)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(18)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(16)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
            <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1,
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(13
            DOWNTO 11)), fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(9)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(8)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(6)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(5)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(3)),
            fsm_output(11));
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1(2)),
            (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(1)),
            fsm_output(11));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
            <= STD_LOGIC_VECTOR'( "000");
      ELSIF ( (core_wen AND nor_55_cse) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse),3)),
            ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32,
            fsm_output(11));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
            <= STD_LOGIC_VECTOR'( "00");
      ELSIF ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_2_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1),
            1),3)), (z_out_4(32 DOWNTO 30)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
            <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (z_out_4(29)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(28)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(27)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (z_out_4(26 DOWNTO 25)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(24)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (z_out_4(23 DOWNTO 22)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(21)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),3)), (z_out_4(20 DOWNTO 18)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(17)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
            <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (z_out_4(16)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
            <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1,
            (z_out_4(15 DOWNTO 14)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),3)), (z_out_4(13 DOWNTO 11)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(10)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (z_out_4(9 DOWNTO 8)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(7)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
            1),2)), (z_out_4(6 DOWNTO 5)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
            <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1(1)),
            (z_out_4(4)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
            <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (z_out_4(3)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
            <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse,
            (z_out_4(2 DOWNTO 1)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_110 OR or_dcpl_109 OR (fsm_output(24)) OR
          (fsm_output(20)) OR or_dcpl_83 OR or_dcpl_82 OR (fsm_output(10)) OR (fsm_output(13))
          OR or_dcpl_103))) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
            <= MUX_v_33_2_2((z_out_6(32 DOWNTO 0)), (z_out_5(32 DOWNTO 0)), fsm_output(9));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_110 OR (fsm_output(25)) OR (fsm_output(16))
          OR (fsm_output(23)) OR (fsm_output(20)) OR or_dcpl_83 OR or_dcpl_82 OR
          (fsm_output(9)) OR (fsm_output(13)) OR or_dcpl_103))) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
            <= MUX_v_33_2_2(z_out_2, (z_out_4(32 DOWNTO 0)), fsm_output(10));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
            <= '0';
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32
            <= STD_LOGIC_VECTOR'( "000");
      ELSIF ( and_1771_itm = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt(32);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt(34
            DOWNTO 32);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( ((NOT((NOT nor_66_cse) OR (fsm_output(6)))) AND core_wen) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt(31
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( ((NOT((fsm_output(6)) OR (fsm_output(11)) OR (fsm_output(7)))) AND
          nor_66_cse AND core_wen) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0
            <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt(31
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
            <= z_out_3(4 DOWNTO 0);
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
            <= z_out_7;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_atan_pi_2mi_return_2_69_38_sva <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT (fsm_output(7)))) = '1' ) THEN
        ac_math_atan_pi_2mi_return_2_69_38_sva <= data_out_out;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
            <= '0';
      ELSIF ( (core_wen AND (NOT(((NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
          OR (T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c)
          OR or_dcpl_131 OR (fsm_output(27)) OR (fsm_output(26)) OR (fsm_output(8))
          OR (fsm_output(7))))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
            <= MUX_s_1_2_2((z_out_4(35)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000,
            ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT((fsm_output(10)) OR (fsm_output(8)) OR (fsm_output(7)))))
          = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm
            <= MUX1HOT_v_33_3_2(z_out_2, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4,
            (z_out_4(32 DOWNTO 0)), STD_LOGIC_VECTOR'( (fsm_output(6)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
            & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
            <= '0';
      ELSIF ( (core_wen AND (fsm_output(7))) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
            <= z_out_4(35);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_77 OR (fsm_output(8))))) = '1' ) THEN
        ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
            <= z_out_5;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
            <= '0';
      ELSIF ( (core_wen AND (NOT((fsm_output(21)) OR (fsm_output(9)) OR (fsm_output(10))
          OR (fsm_output(27)) OR (fsm_output(8))))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_nl,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl,
            ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_nl);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
            <= '0';
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000");
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse
          = '1' ) THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
            <= (z_out_3(26)) AND (NOT(and_dcpl_103 OR and_dcpl_105));
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
            <= MUX1HOT_v_26_3_2((z_out_3(25 DOWNTO 0)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(31
            DOWNTO 6)), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(31
            DOWNTO 6)), STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_149 OR or_dcpl_109 OR (fsm_output(24)) OR
          (fsm_output(17)) OR or_dcpl_141))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
            <= z_out_6(26 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( ((mux_24_nl OR (fsm_output(17)) OR (fsm_output(23))) AND core_wen)
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
            <= MUX_v_27_2_2(STD_LOGIC_VECTOR'("000000000000000000000000000"), (z_out_4(26
            DOWNTO 0)), not_586_nl);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
            <= STD_LOGIC_VECTOR'( "000000000");
      ELSIF ( (core_wen AND (NOT or_dcpl_72)) = '1' ) THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(8
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  T_LINE_if_aelse_not_57_nl <= NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  T_LINE_if_T_LINE_if_and_34_nl <= MUX_v_12_2_2(STD_LOGIC_VECTOR'("000000000000"),
      x1_t_42_31_lpi_3, T_LINE_if_aelse_not_57_nl);
  mux_nl <= MUX_s_1_2_2((fsm_output(23)), T_LINE_if_if_slc_T_LINE_if_acc_8_svs, fsm_output(26));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_17_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_17_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_18_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_18_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_19_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_19_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(8)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_16_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_16_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_17_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_17_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_18_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_18_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_19_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_19_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_17_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_17_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_18_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_18_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl
      <= MUX_s_1_2_2(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_16_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_16_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_17_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_17_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_18_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_18_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_19_nl
      <= MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_19_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  T_LINE_if_T_LINE_if_and_35_nl <= MUX_v_11_2_2(STD_LOGIC_VECTOR'("00000000000"),
      y1_t_42_32_lpi_3, T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  mux_22_nl <= MUX_s_1_2_2((fsm_output(23)), (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs),
      fsm_output(26));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(0)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(24)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(21)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_8_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(17)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_7_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(10)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_6_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(7)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_5_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(4)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_4_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(2)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_3_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(0)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_2_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(24)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_1_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(7)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl <=
      MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(4)),
      fsm_output(11));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0,
      (z_out_4(0)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_104_nl
      <= MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(10)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(9)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      STD_LOGIC_VECTOR'( or_tmp_209 & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_105_nl
      <= MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(17)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(13)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(14)),
      STD_LOGIC_VECTOR'( or_tmp_209 & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_106_nl
      <= MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(10)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(11)),
      STD_LOGIC_VECTOR'( or_tmp_209 & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_nl <=
      ((NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs)
      AND (fsm_output(22))) OR ((NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs)
      AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_1_nl <=
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      AND (fsm_output(22))) OR (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      AND (fsm_output(28)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_107_nl
      <= MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(21)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1(19)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1,
      STD_LOGIC_VECTOR'( or_tmp_209 & (fsm_output(11)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_nl
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_1_nl));
  or_709_nl <= (fsm_output(28)) OR (fsm_output(27)) OR (fsm_output(6)) OR or_dcpl_94;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_108_nl
      <= MUX1HOT_s_1_3_2(T_LINE_slc_T_LINE_acc_6_itm, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(0)),
      (z_out_8(6)), STD_LOGIC_VECTOR'( or_709_nl & (fsm_output(11)) & (fsm_output(26))));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
      <= MUX_s_1_2_2((NOT (z_out_4(27))), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27,
      (z_out_4(27)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
      <= MUX1HOT_s_1_4_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(0)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(0)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(0)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(0)),
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
  T_LINE_if_acc_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(T_LINE_t_7_0_sva),
      8), 9) + SIGNED'( "111010011"), 9));
  T_LINE_if_aelse_acc_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & (NOT (T_LINE_t_7_0_sva(7
      DOWNTO 3)))) + SIGNED'( "010001"), 6));
  T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl <= NOT((T_LINE_if_aelse_acc_nl(5))
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  or_nl <= (fsm_output(4)) OR (fsm_output(14));
  or_1032_nl <= CONV_SL_1_1(fsm_output(11 DOWNTO 5)/=STD_LOGIC_VECTOR'("0000000"));
  mux_23_nl <= MUX_s_1_2_2(or_nl, (fsm_output(14)), or_1032_nl);
  Hough_Algorithm_HW_1296_864_getMaxLine_not_24_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_23_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_19_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_13_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_12_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_7_nl <= NOT (fsm_output(5));
  Hough_Algorithm_HW_1296_864_getMaxLine_not_5_nl <= NOT (fsm_output(5));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000
      <= reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      OR CONV_SL_1_1(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1/=STD_LOGIC_VECTOR'("00000000000000000000000000"));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_nl
      <= NOT(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
      OR (z_out_7(3)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl
      <= NOT(CONV_SL_1_1(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0/=STD_LOGIC_VECTOR'("0000000000000000000000000000")));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_nl <=
      (fsm_output(20)) OR (fsm_output(26));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      <= (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      AND (fsm_output(14))) OR (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      AND (fsm_output(20)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
      <= (and_dcpl_103 AND (fsm_output(14))) OR (and_dcpl_103 AND (fsm_output(20)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl
      <= (and_dcpl_105 AND (fsm_output(14))) OR (and_dcpl_105 AND (fsm_output(20)));
  not_586_nl <= NOT or_tmp_581;
  mux_24_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c,
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  T_LINE_if_T_LINE_if_T_LINE_if_nor_2_nl <= NOT((T_LINE_if_acc_5_psp_1(9)) OR (fsm_output(2))
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse);
  T_LINE_if_mux1h_25_nl <= MUX1HOT_v_19_6_2((STD_LOGIC_VECTOR'( "00000001000") &
      (NOT T_LINE_t_7_0_sva)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((NOT T_LINE_if_acc_5_psp_1)
      & (NOT (T_LINE_if_acc_2_itm_12_4(3 DOWNTO 0))) & (NOT T_LINE_if_acc_1_itm_3_0)),19)),
      (NOT (T_LINE_if_if_dividend1_sva(18 DOWNTO 0))), (NOT (T_LINE_if_else_dividend1_sva(18
      DOWNTO 0))), (NOT (T_LINE_if_if_dividend2_sva(18 DOWNTO 0))), (NOT (T_LINE_if_else_dividend2_sva(18
      DOWNTO 0))), STD_LOGIC_VECTOR'( (fsm_output(2)) & (fsm_output(4)) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse));
  T_LINE_if_T_LINE_if_or_1_nl <= (NOT((fsm_output(2)) OR T_LINE_if_or_ssc)) OR (fsm_output(4));
  T_LINE_if_or_2_nl <= (fsm_output(4)) OR T_LINE_if_or_ssc;
  T_LINE_if_T_LINE_if_T_LINE_if_nor_3_nl <= NOT(MUX_v_8_2_2(T_LINE_t_7_0_sva, STD_LOGIC_VECTOR'("11111111"),
      T_LINE_if_or_2_nl));
  z_out <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(T_LINE_if_T_LINE_if_T_LINE_if_nor_2_nl
      & T_LINE_if_mux1h_25_nl) + CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(T_LINE_if_T_LINE_if_or_1_nl
      & STD_LOGIC_VECTOR'( "0000") & T_LINE_if_T_LINE_if_T_LINE_if_nor_3_nl & STD_LOGIC_VECTOR'(
      "0001")), 17), 20), 20));
  T_LINE_mux1h_4_nl <= MUX1HOT_v_11_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(T_LINE_t_7_0_sva),11)),
      R_LINE_r_10_0_sva, STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((T_LINE_if_acc_1_itm_12_4(1
      DOWNTO 0)) & T_LINE_if_acc_1_itm_3_0),11)), STD_LOGIC_VECTOR'( (fsm_output(26))
      & (fsm_output(30)) & or_948_cse_1));
  z_out_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(T_LINE_mux1h_4_nl) + UNSIGNED'(
      "00000000001"), 11));
  or_1063_nl <= ((NOT((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32))
      AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs)) AND (fsm_output(14))) OR ((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32))
      AND (NOT T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND (fsm_output(20)));
  or_1064_nl <= ((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32))
      AND T_LINE_if_if_slc_T_LINE_if_acc_8_svs AND (fsm_output(14))) OR (((NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32)))
      OR T_LINE_if_if_slc_T_LINE_if_acc_8_svs) AND (fsm_output(20)));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux1h_11_nl
      <= MUX1HOT_v_26_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva),26)),
      (NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(31
      DOWNTO 6))), (NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(31
      DOWNTO 6))), STD_LOGIC_VECTOR'( (fsm_output(6)) & or_1063_nl & or_1064_nl));
  z_out_3 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux1h_11_nl),
      27) + UNSIGNED'( "000000000000000000000000001"), 27));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_5_nl
      <= or_tmp_777 OR or_tmp_778;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_6_nl
      <= or_tmp_779 OR or_tmp_780;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      <= MUX1HOT_v_35_7_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0),35)),
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0),
      ((NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32)
      & (NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
      & T_LINE_slc_T_LINE_acc_6_itm),35)), ((NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32)
      & (NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0),35)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva),35)),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_5_nl
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_6_nl
      & (fsm_output(6)) & (fsm_output(9)) & (fsm_output(7)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt
      & or_948_cse_1));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_7_nl
      <= (NOT(or_tmp_778 OR or_tmp_779 OR (fsm_output(9)) OR ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      OR or_948_cse_1)) OR or_tmp_777 OR or_tmp_780 OR CONV_SL_1_1(fsm_output(7 DOWNTO
      6)/=STD_LOGIC_VECTOR'("00")) OR ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      <= (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva(26))
      AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_8_nl
      <= CONV_SL_1_1(fsm_output(7 DOWNTO 6)/=STD_LOGIC_VECTOR'("00"));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl
      <= MUX1HOT_v_18_9_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
      DOWNTO 16))),18)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
      DOWNTO 16)),18)), (STD_LOGIC_VECTOR'( "00") & (ac_math_atan_pi_2mi_return_2_69_38_sva(31
      DOWNTO 16))), (STD_LOGIC_VECTOR'( "11") & (NOT (ac_math_atan_pi_2mi_return_2_69_38_sva(31
      DOWNTO 16)))), (T_LINE_if_acc_2_itm_13 & T_LINE_if_acc_2_itm_12_4 & ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
      & ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(32
      DOWNTO 16)),18)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32)
      & (NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0(31
      DOWNTO 16)))),18)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
      & (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0(31
      DOWNTO 16))),18)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      & (exs_tmp_16_26_0(26 DOWNTO 16))),18)), STD_LOGIC_VECTOR'( or_tmp_777 & or_tmp_778
      & or_tmp_779 & or_tmp_780 & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_8_nl
      & (fsm_output(9)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      & or_948_cse_1));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl
      <= MUX1HOT_v_16_8_2((NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(15
      DOWNTO 0))), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(15
      DOWNTO 0)), (ac_math_atan_pi_2mi_return_2_69_38_sva(15 DOWNTO 0)), (NOT (ac_math_atan_pi_2mi_return_2_69_38_sva(15
      DOWNTO 0))), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm(15
      DOWNTO 0)), (NOT (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0(15
      DOWNTO 0))), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0(15
      DOWNTO 0)), (exs_tmp_16_26_0(15 DOWNTO 0)), STD_LOGIC_VECTOR'( or_tmp_777 &
      or_tmp_778 & or_tmp_779 & or_tmp_780 & (fsm_output(9)) & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      & or_948_cse_1));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      <= MUX_v_16_2_2(STD_LOGIC_VECTOR'("0000000000000000"), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl,
      nor_55_cse);
  acc_3_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_7_nl),
      36), 37) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      & '1'), 35), 37), 37));
  z_out_4 <= acc_3_nl(36 DOWNTO 1);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_4_nl
      <= or_tmp_789 OR or_tmp_790;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux_1_nl
      <= MUX_v_35_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0),35)),
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_4_nl);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_5_nl
      <= (NOT(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      OR or_tmp_789)) OR ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      OR or_tmp_790;
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_4_nl
      <= MUX1HOT_v_33_4_2(z_out_2, (NOT z_out_2), ('0' & data_out_out), ('1' & (NOT
      data_out_out)), STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      & or_tmp_789 & or_tmp_790));
  acc_4_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux_1_nl
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_5_nl)
      + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_4_nl
      & '1'), 34), 36), 36));
  z_out_5 <= acc_4_nl(35 DOWNTO 1);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_5_nl
      <= MUX1HOT_v_44_4_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
      & T_LINE_slc_T_LINE_acc_6_itm),44)), STD_LOGIC_VECTOR'( "00000000000000000000000000000000000000000001"),
      (NOT T_LINE_if_if_dividend1_mul_cmp_z_oreg), STD_LOGIC_VECTOR'( "11111111111111111111111111111111111111110101"),
      STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_792 & or_tmp_793 & or_948_cse_1));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_6_nl
      <= (NOT(or_tmp_792 OR or_tmp_793 OR or_948_cse_1)) OR (fsm_output(7));
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_6_nl
      <= MUX1HOT_v_40_4_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(NOT ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm),40)),
      (STD_LOGIC_VECTOR'( "0000000000000") & (NOT reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd)
      & (NOT reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1)),
      (T_LINE_if_acc_2_itm_12_4 & (R_LINE_r_10_0_sva(1 DOWNTO 0)) & STD_LOGIC_VECTOR'(
      "00000000000000000000000000001")), (STD_LOGIC_VECTOR'( "000000000000000000000000000000000000")
      & (z_out_1(5 DOWNTO 2))), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_792 &
      or_tmp_793 & or_948_cse_1));
  acc_5_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_5_nl
      & ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_6_nl)
      + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_6_nl
      & '1'), 41), 45), 45));
  z_out_6 <= acc_5_nl(44 DOWNTO 1);
  ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_23_nl
      <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      (STD_LOGIC_VECTOR'( "11") & (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1(4
      DOWNTO 2))), fsm_output(7));
  z_out_7 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_23_nl)
      + UNSIGNED'( "00001"), 5));
  operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      OR or_tmp_800;
  operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      OR or_tmp_802;
  operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl <= MUX1HOT_v_17_9_2(STD_LOGIC_VECTOR'(
      "00000000001010001"), STD_LOGIC_VECTOR'( "00000000000011011"), (STD_LOGIC_VECTOR'(
      "11111111") & T_LINE_if_acc_1_itm_12_4), STD_LOGIC_VECTOR'( "11111111111010011"),
      STD_LOGIC_VECTOR'( "00000000000000001"), STD_LOGIC_VECTOR'( "11111111110101111"),
      ('1' & acc_rsci_idat_mxwt), STD_LOGIC_VECTOR'( "11111111100011011"), STD_LOGIC_VECTOR'(
      "11111111111100101"), STD_LOGIC_VECTOR'( operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl
      & operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl & (fsm_output(3)) & (fsm_output(26))
      & (fsm_output(6)) & T_LINE_if_if_dividend1_or_1_itm & (fsm_output(1)) & (fsm_output(30))
      & T_LINE_if_if_dividend1_or_itm));
  operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl <= (NOT(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      OR or_tmp_800 OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      OR or_tmp_802 OR (fsm_output(3)) OR (fsm_output(26)) OR (fsm_output(6)) OR
      T_LINE_if_if_dividend1_mul_cmp_a_mx0c0 OR T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
      OR (fsm_output(30)) OR T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 OR T_LINE_if_if_dividend1_mul_cmp_a_mx0c3))
      OR (fsm_output(1));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (T_LINE_if_else_dividend1_sva(19)), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_26_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_27_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_28_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_29_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_30_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_31_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (T_LINE_if_else_dividend2_sva(19)), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_26_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_27_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_28_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_29_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_30_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_31_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (T_LINE_if_if_dividend1_sva(19)), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_23_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_24_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_25_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_26_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_27_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_28_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_29_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_30_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (T_LINE_if_if_dividend2_sva(19)), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_23_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_24_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_25_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_26_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_27_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_28_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_29_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_30_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl <= MUX1HOT_v_16_13_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED'(
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_26_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_27_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_28_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_29_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_30_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_31_nl),16)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_26_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_27_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_28_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_29_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_30_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_31_nl),16)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_23_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_24_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_25_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_26_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_27_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_28_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_29_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_30_nl),16)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_23_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_24_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_25_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_26_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_27_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_28_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_29_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_30_nl),16)),
      (STD_LOGIC_VECTOR'( "0000") & T_LINE_t_7_0_sva & STD_LOGIC_VECTOR'( "0001")),
      (STD_LOGIC_VECTOR'( "0000000000") & (z_out_1(7 DOWNTO 2))), (STD_LOGIC_VECTOR'(
      "1111111111111") & (z_out_7(4 DOWNTO 2))), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(x1_t_42_31_lpi_3),16)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(x2_t_42_31_lpi_3),16)), (NOT threshold_23_8_lpi_3),
      (STD_LOGIC_VECTOR'( "00000000") & (z_out_1(10 DOWNTO 3))), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(y1_t_42_32_lpi_3),16)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(y2_t_42_32_lpi_3),16)), STD_LOGIC_VECTOR'(
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      & or_tmp_800 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      & or_tmp_802 & (fsm_output(3)) & (fsm_output(26)) & (fsm_output(6)) & T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      & T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 & (fsm_output(1)) & (fsm_output(30))
      & T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 & T_LINE_if_if_dividend1_mul_cmp_a_mx0c3));
  acc_7_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl
      & operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl) + CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl
      & '1'), 17), 18), 18));
  z_out_8 <= acc_7_nl(17 DOWNTO 1);
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_core
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform_core IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    data_in_rsc_vld : IN STD_LOGIC;
    data_in_rsc_rdy : OUT STD_LOGIC;
    widthIn_rsc_triosy_lz : OUT STD_LOGIC;
    heightIn_rsc_triosy_lz : OUT STD_LOGIC;
    acc_rsc_dat : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsc_vld : OUT STD_LOGIC;
    acc_rsc_rdy : IN STD_LOGIC;
    widthIn_rsci_idat : IN STD_LOGIC_VECTOR (10 DOWNTO 0);
    heightIn_rsci_idat : IN STD_LOGIC_VECTOR (9 DOWNTO 0);
    acc_tmp_rsci_data_in_d : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_tmp_rsci_addr_d : OUT STD_LOGIC_VECTOR (18 DOWNTO 0);
    acc_tmp_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    acc_tmp_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    acc_tmp_rsci_data_out_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
    acc_tmp_rsci_en_d : OUT STD_LOGIC
  );
END houghTransform_core;

ARCHITECTURE v1 OF houghTransform_core IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL core_wen : STD_LOGIC;
  SIGNAL core_wten : STD_LOGIC;
  SIGNAL data_in_rsci_wen_comp : STD_LOGIC;
  SIGNAL data_in_rsci_idat_mxwt : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL acc_rsci_wen_comp : STD_LOGIC;
  SIGNAL acc_rsci_idat : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_out_d_oreg : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL fsm_output : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL HROW_equal_tmp : STD_LOGIC;
  SIGNAL HCOL_equal_tmp : STD_LOGIC;
  SIGNAL for_for_nor_tmp : STD_LOGIC;
  SIGNAL and_dcpl_13 : STD_LOGIC;
  SIGNAL or_dcpl_22 : STD_LOGIC;
  SIGNAL or_dcpl_33 : STD_LOGIC;
  SIGNAL or_dcpl_34 : STD_LOGIC;
  SIGNAL or_dcpl_35 : STD_LOGIC;
  SIGNAL or_dcpl_36 : STD_LOGIC;
  SIGNAL or_dcpl_54 : STD_LOGIC;
  SIGNAL or_dcpl_58 : STD_LOGIC;
  SIGNAL or_dcpl_60 : STD_LOGIC;
  SIGNAL or_dcpl_61 : STD_LOGIC;
  SIGNAL and_dcpl_32 : STD_LOGIC;
  SIGNAL or_dcpl_73 : STD_LOGIC;
  SIGNAL or_tmp_57 : STD_LOGIC;
  SIGNAL or_tmp_77 : STD_LOGIC;
  SIGNAL or_tmp_167 : STD_LOGIC;
  SIGNAL or_tmp_302 : STD_LOGIC;
  SIGNAL or_tmp_306 : STD_LOGIC;
  SIGNAL or_tmp_404 : STD_LOGIC;
  SIGNAL or_tmp_419 : STD_LOGIC;
  SIGNAL or_tmp_420 : STD_LOGIC;
  SIGNAL and_137_cse : STD_LOGIC;
  SIGNAL and_186_cse : STD_LOGIC;
  SIGNAL and_198_cse : STD_LOGIC;
  SIGNAL and_195_cse : STD_LOGIC;
  SIGNAL and_435_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm
      : STD_LOGIC;
  SIGNAL HACC_slc_HACC_acc_6_itm : STD_LOGIC;
  SIGNAL HACC_idx_HACC_idx_acc_conv_2f_and_itm : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL HACC_mul_2_itm : STD_LOGIC_VECTOR (38 DOWNTO 0);
  SIGNAL reg_HACC_slc_HACC_acc_7_39_14_psp_ftd : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL or_131_ssc : STD_LOGIC;
  SIGNAL or_134_ssc : STD_LOGIC;
  SIGNAL reg_heightIn_rsc_triosy_obj_ld_core_psct_cse : STD_LOGIC;
  SIGNAL reg_acc_rsci_ivld_core_psct_cse : STD_LOGIC;
  SIGNAL reg_data_in_rsci_irdy_core_psct_cse : STD_LOGIC;
  SIGNAL reg_acc_tmp_rsc_cgo_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse
      : STD_LOGIC;
  SIGNAL and_143_cse : STD_LOGIC;
  SIGNAL HACC_idx_acc_8_psp : STD_LOGIC_VECTOR (14 DOWNTO 0);
  SIGNAL HACC_acc_10_psp : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL or_cse : STD_LOGIC;
  SIGNAL or_125_rmff : STD_LOGIC;
  SIGNAL HACC_t_7_0_sva : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL z_out_1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL z_out_2 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL z_out_3 : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL z_out_4 : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL or_tmp_463 : STD_LOGIC;
  SIGNAL or_tmp_464 : STD_LOGIC;
  SIGNAL z_out_5 : STD_LOGIC_VECTOR (39 DOWNTO 0);
  SIGNAL or_tmp_475 : STD_LOGIC;
  SIGNAL or_tmp_476 : STD_LOGIC;
  SIGNAL z_out_6 : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL z_out_7 : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL z_out_8 : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL z_out_9 : STD_LOGIC_VECTOR (38 DOWNTO 0);
  SIGNAL data_out_out : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL operator_11_false_io_read_widthIn_rsc_cse_sva : STD_LOGIC_VECTOR (10 DOWNTO
      0);
  SIGNAL operator_10_false_io_read_heightIn_rsc_cse_sva : STD_LOGIC_VECTOR (9 DOWNTO
      0);
  SIGNAL acc_tmp_vinit_ndx_sva : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL HROW_y_sva : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_return_69_38_sva : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL HACC_t_7_0_sva_1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL HACC_mul_3_itm : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0
      : STD_LOGIC;
  SIGNAL HACC_idx_acc_3_psp_sva_mx0c0 : STD_LOGIC;
  SIGNAL HACC_idx_acc_3_psp_sva_mx0c3 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_conc_3_itm_18_0 : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL for_conc_4_itm_18_0 : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse
      : STD_LOGIC;
  SIGNAL HACC_t_or_cse : STD_LOGIC;
  SIGNAL and_954_cse : STD_LOGIC;
  SIGNAL and_956_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
      : STD_LOGIC;
  SIGNAL or_164_ssc : STD_LOGIC;
  SIGNAL HACC_idx_and_ssc : STD_LOGIC;
  SIGNAL HACC_idx_and_ssc_2 : STD_LOGIC;
  SIGNAL reg_HACC_idx_acc_3_psp_ftd : STD_LOGIC;
  SIGNAL reg_HACC_idx_acc_3_psp_ftd_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL reg_HACC_idx_acc_3_psp_ftd_2 : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL reg_HACC_idx_acc_3_psp_ftd_3 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm
      : STD_LOGIC;
  SIGNAL for_1_nor_2_seb : STD_LOGIC;
  SIGNAL or_379_cse : STD_LOGIC;
  SIGNAL z_out_6_2 : STD_LOGIC_VECTOR (4 DOWNTO 0);

  SIGNAL for_and_nl : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL for_mux1h_6_nl : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL HCOL_x_HCOL_x_and_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL HCOL_x_mux_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL HCOL_x_nor_nl : STD_LOGIC;
  SIGNAL or_140_nl : STD_LOGIC;
  SIGNAL or_143_nl : STD_LOGIC;
  SIGNAL for_nor_nl : STD_LOGIC;
  SIGNAL or_568_nl : STD_LOGIC;
  SIGNAL HACC_mux1h_8_nl : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL HACC_acc_5_nl : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL or_516_nl : STD_LOGIC;
  SIGNAL or_163_nl : STD_LOGIC;
  SIGNAL HACC_mux_1_nl : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL HACC_acc_12_nl : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL or_165_nl : STD_LOGIC;
  SIGNAL or_462_nl : STD_LOGIC;
  SIGNAL T_LINE_if_aelse_mux_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_mux_1_nl
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL not_224_nl : STD_LOGIC;
  SIGNAL T_LINE_if_aelse_mux1h_17_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_1_nl
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_1_nl
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL or_484_nl : STD_LOGIC;
  SIGNAL and_nl : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL not_226_nl : STD_LOGIC;
  SIGNAL or_486_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL HACC_idx_HACC_idx_acc_conv_2f_or_nl : STD_LOGIC;
  SIGNAL HACC_idx_HACC_idx_acc_conv_2f_and_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_y_2mi_mux1h_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL or_497_nl : STD_LOGIC;
  SIGNAL or_506_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL for_nor_1_nl : STD_LOGIC;
  SIGNAL for_nor_2_nl : STD_LOGIC;
  SIGNAL for_mux1h_2_nl : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL for_mux1h_7_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_or_nl : STD_LOGIC;
  SIGNAL HACC_mux_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HACC_idx_or_1_nl : STD_LOGIC;
  SIGNAL HACC_idx_or_2_nl : STD_LOGIC;
  SIGNAL HACC_idx_or_3_nl : STD_LOGIC;
  SIGNAL HACC_acc_nl : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL HACC_mux_7_nl : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL HACC_mux_8_nl : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_3_nl
      : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL acc_2_nl : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL HCOL_and_2_nl : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL HCOL_mux1h_4_nl : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL HCOL_nor_3_nl : STD_LOGIC;
  SIGNAL HCOL_or_4_nl : STD_LOGIC;
  SIGNAL HCOL_mux1h_5_nl : STD_LOGIC;
  SIGNAL HCOL_mux1h_6_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL HCOL_or_5_nl : STD_LOGIC;
  SIGNAL HCOL_HCOL_nand_1_nl : STD_LOGIC_VECTOR (14 DOWNTO 0);
  SIGNAL HCOL_mux1h_7_nl : STD_LOGIC_VECTOR (14 DOWNTO 0);
  SIGNAL HCOL_or_6_nl : STD_LOGIC;
  SIGNAL HCOL_not_6_nl : STD_LOGIC;
  SIGNAL for_1_mux1h_5_nl : STD_LOGIC_VECTOR (17 DOWNTO 0);
  SIGNAL for_1_for_1_or_2_nl : STD_LOGIC;
  SIGNAL for_1_for_1_and_2_nl : STD_LOGIC;
  SIGNAL for_1_for_1_and_3_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_1_for_1_or_3_nl : STD_LOGIC;
  SIGNAL acc_4_nl : STD_LOGIC_VECTOR (33 DOWNTO 0);
  SIGNAL HROW_HROW_and_13_nl : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL HROW_HROW_and_14_nl : STD_LOGIC;
  SIGNAL HROW_HROW_and_15_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HROW_HROW_and_16_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HROW_HROW_and_17_nl : STD_LOGIC;
  SIGNAL HROW_HROW_and_18_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HROW_HROW_and_19_nl : STD_LOGIC;
  SIGNAL HROW_HROW_and_20_nl : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL HROW_HROW_and_21_nl : STD_LOGIC;
  SIGNAL HROW_HROW_and_22_nl : STD_LOGIC;
  SIGNAL HROW_HROW_and_23_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HROW_HROW_and_24_nl : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL HROW_HROW_and_25_nl : STD_LOGIC;
  SIGNAL HROW_mux_9_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HROW_mux_10_nl : STD_LOGIC;
  SIGNAL HROW_mux_11_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HROW_mux_12_nl : STD_LOGIC;
  SIGNAL HROW_mux_13_nl : STD_LOGIC;
  SIGNAL HROW_mux_14_nl : STD_LOGIC;
  SIGNAL HROW_mux_15_nl : STD_LOGIC;
  SIGNAL HROW_mux_16_nl : STD_LOGIC;
  SIGNAL HROW_or_1_nl : STD_LOGIC;
  SIGNAL HROW_mux_17_nl : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL acc_5_nl : STD_LOGIC_VECTOR (40 DOWNTO 0);
  SIGNAL for_1_mux1h_6_nl : STD_LOGIC_VECTOR (38 DOWNTO 0);
  SIGNAL for_1_or_8_nl : STD_LOGIC;
  SIGNAL for_1_or_9_nl : STD_LOGIC;
  SIGNAL for_1_or_10_nl : STD_LOGIC;
  SIGNAL for_1_or_11_nl : STD_LOGIC_VECTOR (21 DOWNTO 0);
  SIGNAL for_1_and_4_nl : STD_LOGIC_VECTOR (21 DOWNTO 0);
  SIGNAL for_1_mux1h_7_nl : STD_LOGIC_VECTOR (21 DOWNTO 0);
  SIGNAL for_1_not_6_nl : STD_LOGIC;
  SIGNAL for_1_or_12_nl : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL for_1_and_5_nl : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL for_1_mux1h_8_nl : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL for_1_nor_6_nl : STD_LOGIC;
  SIGNAL acc_6_nl : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_30_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_18_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_32_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_27_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_27_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_28_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_28_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_18_nl
      : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_33_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_19_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL HACC_idx_acc_9_nl : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_13_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_45_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_47_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_48_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_50_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_52_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_55_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_56_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_9_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_11_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL HACC_mux_9_nl : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL HACC_mux_10_nl : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a : STD_LOGIC_VECTOR (32 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s : STD_LOGIC_VECTOR (4 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z : STD_LOGIC_VECTOR (32 DOWNTO
      0);

  SIGNAL ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
      : STD_LOGIC_VECTOR (31 DOWNTO 0);

  COMPONENT houghTransform_core_data_in_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
      data_in_rsc_vld : IN STD_LOGIC;
      data_in_rsc_rdy : OUT STD_LOGIC;
      core_wen : IN STD_LOGIC;
      data_in_rsci_oswt : IN STD_LOGIC;
      data_in_rsci_wen_comp : OUT STD_LOGIC;
      data_in_rsci_idat_mxwt : OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL houghTransform_core_data_in_rsci_inst_data_in_rsc_dat : STD_LOGIC_VECTOR
      (7 DOWNTO 0);
  SIGNAL houghTransform_core_data_in_rsci_inst_data_in_rsci_idat_mxwt : STD_LOGIC_VECTOR
      (7 DOWNTO 0);

  COMPONENT houghTransform_core_acc_rsci
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      acc_rsc_dat : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsc_vld : OUT STD_LOGIC;
      acc_rsc_rdy : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      acc_rsci_oswt : IN STD_LOGIC;
      acc_rsci_wen_comp : OUT STD_LOGIC;
      acc_rsci_idat : IN STD_LOGIC_VECTOR (15 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL houghTransform_core_acc_rsci_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO
      0);
  SIGNAL houghTransform_core_acc_rsci_inst_acc_rsci_idat : STD_LOGIC_VECTOR (15 DOWNTO
      0);

  COMPONENT houghTransform_core_wait_dp
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      acc_tmp_rsc_cgo_iro : IN STD_LOGIC;
      acc_tmp_rsci_data_out_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
      acc_tmp_rsci_en_d : OUT STD_LOGIC;
      core_wen : IN STD_LOGIC;
      acc_tmp_rsc_cgo : IN STD_LOGIC;
      acc_tmp_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d : STD_LOGIC_VECTOR
      (31 DOWNTO 0);
  SIGNAL houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d_oreg : STD_LOGIC_VECTOR
      (15 DOWNTO 0);

  COMPONENT houghTransform_core_widthIn_rsc_triosy_obj
    PORT(
      widthIn_rsc_triosy_lz : OUT STD_LOGIC;
      core_wten : IN STD_LOGIC;
      widthIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT houghTransform_core_heightIn_rsc_triosy_obj
    PORT(
      heightIn_rsc_triosy_lz : OUT STD_LOGIC;
      core_wten : IN STD_LOGIC;
      heightIn_rsc_triosy_obj_iswt0 : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT houghTransform_core_staller
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      core_wen : OUT STD_LOGIC;
      core_wten : OUT STD_LOGIC;
      data_in_rsci_wen_comp : IN STD_LOGIC;
      acc_rsci_wen_comp : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT houghTransform_core_core_fsm
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      core_wen : IN STD_LOGIC;
      fsm_output : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
      acc_tmp_vinit_C_0_tr0 : IN STD_LOGIC;
      HCOL_C_0_tr0 : IN STD_LOGIC;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
          : IN STD_LOGIC;
      HACC_C_15_tr0 : IN STD_LOGIC;
      HCOL_C_1_tr0 : IN STD_LOGIC;
      HROW_C_0_tr0 : IN STD_LOGIC;
      for_1_C_2_tr0 : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL houghTransform_core_core_fsm_inst_fsm_output : STD_LOGIC_VECTOR (31 DOWNTO
      0);
  SIGNAL houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 : STD_LOGIC;
  SIGNAL houghTransform_core_core_fsm_inst_HCOL_C_0_tr0 : STD_LOGIC;
  SIGNAL houghTransform_core_core_fsm_inst_HACC_C_15_tr0 : STD_LOGIC;
  SIGNAL houghTransform_core_core_fsm_inst_for_1_C_2_tr0 : STD_LOGIC;

  FUNCTION CONV_SL_1_1(input_val:BOOLEAN)
  RETURN STD_LOGIC IS
  BEGIN
    IF input_val THEN RETURN '1';ELSE RETURN '0';END IF;
  END;

  FUNCTION MUX1HOT_s_1_3_2(input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_s_1_4_2(input_3 : STD_LOGIC;
  input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
      tmp := sel(3);
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_s_1_6_2(input_5 : STD_LOGIC;
  input_4 : STD_LOGIC;
  input_3 : STD_LOGIC;
  input_2 : STD_LOGIC;
  input_1 : STD_LOGIC;
  input_0 : STD_LOGIC;
  sel : STD_LOGIC_VECTOR(5 DOWNTO 0))
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;
    VARIABLE tmp : STD_LOGIC;

    BEGIN
      tmp := sel(0);
      result := input_0 and tmp;
      tmp := sel(1);
      result := result or ( input_1 and tmp);
      tmp := sel(2);
      result := result or ( input_2 and tmp);
      tmp := sel(3);
      result := result or ( input_3 and tmp);
      tmp := sel(4);
      result := result or ( input_4 and tmp);
      tmp := sel(5);
      result := result or ( input_5 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_11_10_2(input_9 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_8 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_7 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(9 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(10 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(10 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
      tmp := (OTHERS=>sel( 8));
      result := result or ( input_8 and tmp);
      tmp := (OTHERS=>sel( 9));
      result := result or ( input_9 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_11_3_2(input_2 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(10 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(10 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_12_3_2(input_2 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(11 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(11 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_13_3_2(input_2 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(12 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(12 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_15_7_2(input_6 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(6 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(14 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(14 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_16_3_2(input_2 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_16_8_2(input_7 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(7 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_17_4_2(input_3 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(16 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(16 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_18_4_2(input_3 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(17 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(17 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(17 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_19_3_2(input_2 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(18 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(18 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_22_8_2(input_7 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(7 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(21 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(21 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_2_3_2(input_2 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(1 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(1 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_2_4_2(input_3 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(1 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(1 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_33_3_2(input_2 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_33_4_2(input_3 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_33_5_2(input_4 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(4 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_35_3_2(input_2 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(34 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(34 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_39_3_2(input_2 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(38 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(38 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_39_7_2(input_6 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(38 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(6 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(38 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(38 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
      tmp := (OTHERS=>sel( 4));
      result := result or ( input_4 and tmp);
      tmp := (OTHERS=>sel( 5));
      result := result or ( input_5 and tmp);
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_3_3_2(input_2 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(2 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(2 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_3_4_2(input_3 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(2 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(2 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_4_4_2(input_3 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(3 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(3 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
      tmp := (OTHERS=>sel( 3));
      result := result or ( input_3 and tmp);
    RETURN result;
  END;

  FUNCTION MUX_s_1_2_2(input_0 : STD_LOGIC;
  input_1 : STD_LOGIC;
  sel : STD_LOGIC)
  RETURN STD_LOGIC IS
    VARIABLE result : STD_LOGIC;

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_10_2_2(input_0 : STD_LOGIC_VECTOR(9 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(9 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(9 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_11_2_2(input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(10 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_12_2_2(input_0 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(11 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(11 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_15_2_2(input_0 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(14 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(14 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_16_2_2(input_0 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(15 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(15 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_19_2_2(input_0 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(18 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_22_2_2(input_0 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(21 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(21 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_27_2_2(input_0 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(26 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_2_2_2(input_0 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(1 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(1 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_32_2_2(input_0 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(31 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_33_2_2(input_0 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(32 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(32 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_38_2_2(input_0 : STD_LOGIC_VECTOR(37 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(37 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(37 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_3_2_2(input_0 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(2 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(2 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_4_2_2(input_0 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(3 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_5_2_2(input_0 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(4 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_6_2_2(input_0 : STD_LOGIC_VECTOR(5 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(5 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(5 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_7_2_2(input_0 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(6 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_8_2_2(input_0 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(7 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

BEGIN
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg : work.mgc_shift_comps_v5.mgc_shift_r_v5
    GENERIC MAP(
      width_a => 33,
      signd_a => 1,
      width_s => 5,
      width_z => 33
      )
    PORT MAP(
      a => operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a,
      s => operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s,
      z => operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z
    );
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a <= (MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5),
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3,
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_v_2_4_2(STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1,
      STD_LOGIC_VECTOR'( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1),
      STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8)) & (fsm_output(9)) & (fsm_output(11)))))
      & (MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0,
      HACC_slc_HACC_acc_6_itm, STD_LOGIC_VECTOR'( (fsm_output(10)) & (fsm_output(8))
      & (fsm_output(9)) & (fsm_output(11)))));
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      (fsm_output(8)) OR (fsm_output(11)));
  z_out_8 <= operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z;

  ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg : work.hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60_pkg.hough_algorithm_hw_1296_864mgc_rom_22_70_32_1_60
    PORT MAP(
      addr => ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr,
      data_out => ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
    );
  ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr <=
      STD_LOGIC_VECTOR(UNSIGNED'( "00") & UNSIGNED(MUX_v_5_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      fsm_output(9))));
  data_out_out <= ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out;

  houghTransform_core_data_in_rsci_inst : houghTransform_core_data_in_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      data_in_rsc_dat => houghTransform_core_data_in_rsci_inst_data_in_rsc_dat,
      data_in_rsc_vld => data_in_rsc_vld,
      data_in_rsc_rdy => data_in_rsc_rdy,
      core_wen => core_wen,
      data_in_rsci_oswt => reg_data_in_rsci_irdy_core_psct_cse,
      data_in_rsci_wen_comp => data_in_rsci_wen_comp,
      data_in_rsci_idat_mxwt => houghTransform_core_data_in_rsci_inst_data_in_rsci_idat_mxwt
    );
  houghTransform_core_data_in_rsci_inst_data_in_rsc_dat <= data_in_rsc_dat;
  data_in_rsci_idat_mxwt <= houghTransform_core_data_in_rsci_inst_data_in_rsci_idat_mxwt;

  houghTransform_core_acc_rsci_inst : houghTransform_core_acc_rsci
    PORT MAP(
      clk => clk,
      rst => rst,
      acc_rsc_dat => houghTransform_core_acc_rsci_inst_acc_rsc_dat,
      acc_rsc_vld => acc_rsc_vld,
      acc_rsc_rdy => acc_rsc_rdy,
      core_wen => core_wen,
      acc_rsci_oswt => reg_acc_rsci_ivld_core_psct_cse,
      acc_rsci_wen_comp => acc_rsci_wen_comp,
      acc_rsci_idat => houghTransform_core_acc_rsci_inst_acc_rsci_idat
    );
  acc_rsc_dat <= houghTransform_core_acc_rsci_inst_acc_rsc_dat;
  houghTransform_core_acc_rsci_inst_acc_rsci_idat <= acc_rsci_idat;

  houghTransform_core_wait_dp_inst : houghTransform_core_wait_dp
    PORT MAP(
      clk => clk,
      rst => rst,
      acc_tmp_rsc_cgo_iro => or_125_rmff,
      acc_tmp_rsci_data_out_d => houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d,
      acc_tmp_rsci_en_d => acc_tmp_rsci_en_d,
      core_wen => core_wen,
      acc_tmp_rsc_cgo => reg_acc_tmp_rsc_cgo_cse,
      acc_tmp_rsci_data_out_d_oreg => houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d_oreg
    );
  houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d <= acc_tmp_rsci_data_out_d;
  acc_tmp_rsci_data_out_d_oreg <= houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d_oreg;

  houghTransform_core_widthIn_rsc_triosy_obj_inst : houghTransform_core_widthIn_rsc_triosy_obj
    PORT MAP(
      widthIn_rsc_triosy_lz => widthIn_rsc_triosy_lz,
      core_wten => core_wten,
      widthIn_rsc_triosy_obj_iswt0 => reg_heightIn_rsc_triosy_obj_ld_core_psct_cse
    );
  houghTransform_core_heightIn_rsc_triosy_obj_inst : houghTransform_core_heightIn_rsc_triosy_obj
    PORT MAP(
      heightIn_rsc_triosy_lz => heightIn_rsc_triosy_lz,
      core_wten => core_wten,
      heightIn_rsc_triosy_obj_iswt0 => reg_heightIn_rsc_triosy_obj_ld_core_psct_cse
    );
  houghTransform_core_staller_inst : houghTransform_core_staller
    PORT MAP(
      clk => clk,
      rst => rst,
      core_wen => core_wen,
      core_wten => core_wten,
      data_in_rsci_wen_comp => data_in_rsci_wen_comp,
      acc_rsci_wen_comp => acc_rsci_wen_comp
    );
  houghTransform_core_core_fsm_inst : houghTransform_core_core_fsm
    PORT MAP(
      clk => clk,
      rst => rst,
      core_wen => core_wen,
      fsm_output => houghTransform_core_core_fsm_inst_fsm_output,
      acc_tmp_vinit_C_0_tr0 => houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0,
      HCOL_C_0_tr0 => houghTransform_core_core_fsm_inst_HCOL_C_0_tr0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
          => ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm,
      HACC_C_15_tr0 => houghTransform_core_core_fsm_inst_HACC_C_15_tr0,
      HCOL_C_1_tr0 => HCOL_equal_tmp,
      HROW_C_0_tr0 => HROW_equal_tmp,
      for_1_C_2_tr0 => houghTransform_core_core_fsm_inst_for_1_C_2_tr0
    );
  fsm_output <= houghTransform_core_core_fsm_inst_fsm_output;
  houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 <= for_for_nor_tmp;
  houghTransform_core_core_fsm_inst_HCOL_C_0_tr0 <= NOT (z_out_3(8));
  houghTransform_core_core_fsm_inst_HACC_C_15_tr0 <= NOT HACC_slc_HACC_acc_6_itm;
  houghTransform_core_core_fsm_inst_for_1_C_2_tr0 <= NOT HACC_slc_HACC_acc_6_itm;

  or_125_rmff <= (fsm_output(23)) OR (fsm_output(24)) OR (fsm_output(22)) OR (fsm_output(28))
      OR (fsm_output(1)) OR (fsm_output(25)) OR (fsm_output(2)) OR and_186_cse OR
      and_143_cse;
  and_143_cse <= HACC_slc_HACC_acc_6_itm AND (fsm_output(30));
  HACC_t_or_cse <= (fsm_output(25)) OR (fsm_output(3));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_cse <=
      core_wen AND ((fsm_output(7)) OR (fsm_output(13)));
  or_379_cse <= CONV_SL_1_1(fsm_output(9 DOWNTO 8)/=STD_LOGIC_VECTOR'("00"));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      AND (fsm_output(13));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      <= (NOT ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs)
      AND (fsm_output(13));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      OR ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse;
  or_cse <= (fsm_output(8)) OR (fsm_output(13));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
      <= core_wen AND (NOT(or_dcpl_35 OR or_dcpl_33));
  and_954_cse <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      AND (fsm_output(12));
  and_956_cse <= (NOT ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs)
      AND (fsm_output(12));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
      <= and_954_cse OR and_956_cse;
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse <=
      core_wen AND (fsm_output(9));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm <=
      or_tmp_419 OR or_tmp_420;
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0
      <= (z_out_6_2(0)) AND (NOT (z_out_3(18)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0
      <= NOT((z_out_6_2(0)) OR (z_out_3(18)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(z_out_6_2(0), 1),2)),
      STD_LOGIC_VECTOR'( "01"), z_out_3(18));
  HROW_equal_tmp <= CONV_SL_1_1(UNSIGNED(HROW_y_sva) = UNSIGNED(z_out_7(9 DOWNTO
      0)));
  HCOL_equal_tmp <= CONV_SL_1_1(UNSIGNED(acc_tmp_vinit_ndx_sva(10 DOWNTO 0)) = UNSIGNED(z_out_2(10
      DOWNTO 0)));
  for_for_nor_tmp <= NOT(CONV_SL_1_1(acc_tmp_vinit_ndx_sva/=STD_LOGIC_VECTOR'("0000000000000000000")));
  and_137_cse <= (NOT HROW_equal_tmp) AND (fsm_output(27));
  and_dcpl_13 <= NOT((fsm_output(0)) OR (fsm_output(31)));
  or_dcpl_22 <= (fsm_output(0)) OR (fsm_output(31));
  or_dcpl_33 <= CONV_SL_1_1(fsm_output(11 DOWNTO 10)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_34 <= or_dcpl_33 OR or_cse;
  or_dcpl_35 <= (fsm_output(9)) OR (fsm_output(12));
  or_dcpl_36 <= CONV_SL_1_1(fsm_output(7 DOWNTO 6)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_54 <= CONV_SL_1_1(fsm_output(21 DOWNTO 20)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_58 <= (fsm_output(10)) OR (fsm_output(8));
  or_dcpl_60 <= CONV_SL_1_1(fsm_output(12 DOWNTO 11)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_61 <= (fsm_output(7)) OR (fsm_output(9));
  and_dcpl_32 <= NOT((fsm_output(10)) OR (fsm_output(8)) OR (fsm_output(13)));
  or_dcpl_73 <= CONV_SL_1_1(fsm_output(14 DOWNTO 13)/=STD_LOGIC_VECTOR'("00"));
  and_186_cse <= HROW_equal_tmp AND (fsm_output(27));
  and_195_cse <= for_for_nor_tmp AND (fsm_output(2));
  and_198_cse <= (NOT for_for_nor_tmp) AND (fsm_output(2));
  or_tmp_57 <= and_dcpl_13 AND (NOT (fsm_output(28))) AND (NOT((fsm_output(29)) OR
      (fsm_output(1)))) AND (NOT (fsm_output(30)));
  or_tmp_77 <= or_dcpl_36 OR (fsm_output(5)) OR or_dcpl_35 OR or_dcpl_34;
  and_435_cse <= (z_out_3(18)) AND (fsm_output(7));
  or_tmp_167 <= (NOT (z_out_3(18))) AND (fsm_output(7));
  or_tmp_302 <= NOT((fsm_output(9)) OR (fsm_output(8)) OR (fsm_output(13)));
  or_tmp_306 <= or_dcpl_35 OR (fsm_output(11)) OR or_dcpl_58;
  or_tmp_404 <= or_dcpl_60 OR (fsm_output(10));
  or_tmp_419 <= HACC_idx_HACC_idx_acc_conv_2f_and_itm AND (fsm_output(11));
  or_tmp_420 <= (NOT HACC_idx_HACC_idx_acc_conv_2f_and_itm) AND (fsm_output(11));
  HACC_idx_acc_3_psp_sva_mx0c0 <= (fsm_output(3)) OR (fsm_output(25)) OR (fsm_output(14));
  HACC_idx_acc_3_psp_sva_mx0c3 <= CONV_SL_1_1(fsm_output(21 DOWNTO 19)/=STD_LOGIC_VECTOR'("000"));
  for_conc_3_itm_18_0 <= MUX_v_19_2_2(STD_LOGIC_VECTOR'( "1100001101001111111"),
      (z_out_5(18 DOWNTO 0)), fsm_output(2));
  for_conc_4_itm_18_0 <= MUX_v_19_2_2(STD_LOGIC_VECTOR'("0000000000000000000"), acc_tmp_vinit_ndx_sva,
      (fsm_output(30)));
  or_131_ssc <= CONV_SL_1_1(fsm_output(2 DOWNTO 1)/=STD_LOGIC_VECTOR'("00"));
  or_134_ssc <= (fsm_output(30)) OR (fsm_output(27));
  for_nor_1_nl <= NOT(and_186_cse OR and_143_cse OR (fsm_output(22)));
  acc_tmp_rsci_re_d <= STD_LOGIC_VECTOR'( '1' & for_nor_1_nl);
  for_nor_2_nl <= NOT((fsm_output(24)) OR (fsm_output(1)) OR and_198_cse);
  acc_tmp_rsci_we_d <= STD_LOGIC_VECTOR'( '1' & for_nor_2_nl);
  acc_tmp_rsci_data_in_d <= MUX_v_16_2_2(STD_LOGIC_VECTOR'("0000000000000000"), z_out_2,
      (fsm_output(24)));
  for_mux1h_2_nl <= MUX1HOT_v_17_4_2((for_conc_3_itm_18_0(18 DOWNTO 2)), (z_out_6(16
      DOWNTO 0)), (reg_HACC_idx_acc_3_psp_ftd & reg_HACC_idx_acc_3_psp_ftd_1 & reg_HACC_idx_acc_3_psp_ftd_2
      & reg_HACC_idx_acc_3_psp_ftd_3), (for_conc_4_itm_18_0(18 DOWNTO 2)), STD_LOGIC_VECTOR'(
      or_131_ssc & (fsm_output(22)) & (fsm_output(24)) & or_134_ssc));
  for_or_nl <= (fsm_output(22)) OR (fsm_output(24));
  for_mux1h_7_nl <= MUX1HOT_v_2_3_2((for_conc_3_itm_18_0(1 DOWNTO 0)), (HACC_t_7_0_sva(1
      DOWNTO 0)), (for_conc_4_itm_18_0(1 DOWNTO 0)), STD_LOGIC_VECTOR'( or_131_ssc
      & for_or_nl & or_134_ssc));
  acc_tmp_rsci_addr_d <= for_mux1h_2_nl & for_mux1h_7_nl;
  or_tmp_463 <= (NOT ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs)
      AND (fsm_output(10));
  or_tmp_464 <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      AND (fsm_output(10));
  or_tmp_475 <= (NOT HACC_idx_HACC_idx_acc_conv_2f_and_itm) AND (fsm_output(9));
  or_tmp_476 <= HACC_idx_HACC_idx_acc_conv_2f_and_itm AND (fsm_output(9));
  or_164_ssc <= or_dcpl_36 OR or_dcpl_35 OR or_dcpl_34;
  HACC_idx_and_ssc <= (NOT or_dcpl_54) AND HACC_idx_acc_3_psp_sva_mx0c3;
  HACC_idx_and_ssc_2 <= core_wen AND (HACC_idx_acc_3_psp_sva_mx0c0 OR or_tmp_77 OR
      (fsm_output(18)) OR HACC_idx_acc_3_psp_sva_mx0c3 OR (fsm_output(22)));
  for_1_nor_2_seb <= NOT(CONV_SL_1_1(fsm_output(8 DOWNTO 7)/=STD_LOGIC_VECTOR'("00")));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm
      <= or_tmp_475 OR or_tmp_476;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= '0';
        reg_acc_tmp_rsc_cgo_cse <= '0';
        reg_acc_rsci_ivld_core_psct_cse <= '0';
        reg_data_in_rsci_irdy_core_psct_cse <= '0';
        operator_10_false_io_read_heightIn_rsc_cse_sva <= STD_LOGIC_VECTOR'( "0000000000");
        operator_11_false_io_read_widthIn_rsc_cse_sva <= STD_LOGIC_VECTOR'( "00000000000");
        acc_tmp_vinit_ndx_sva <= STD_LOGIC_VECTOR'( "0000000000000000000");
        HACC_acc_10_psp <= STD_LOGIC_VECTOR'( "0000000000000");
        HACC_idx_acc_8_psp <= STD_LOGIC_VECTOR'( "000000000000000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
            <= STD_LOGIC_VECTOR'( "00000");
        HACC_mul_2_itm <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000000000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
            <= '0';
        ac_math_atan_pi_2mi_return_69_38_sva <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( core_wen = '1' ) THEN
        reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= (NOT HACC_slc_HACC_acc_6_itm)
            AND (fsm_output(30));
        reg_acc_tmp_rsc_cgo_cse <= or_125_rmff;
        reg_acc_rsci_ivld_core_psct_cse <= fsm_output(29);
        reg_data_in_rsci_irdy_core_psct_cse <= and_137_cse OR ((NOT HCOL_equal_tmp)
            AND (fsm_output(26))) OR and_195_cse;
        operator_10_false_io_read_heightIn_rsc_cse_sva <= MUX_v_10_2_2(heightIn_rsci_idat,
            operator_10_false_io_read_heightIn_rsc_cse_sva, or_tmp_57);
        operator_11_false_io_read_widthIn_rsc_cse_sva <= MUX_v_11_2_2(widthIn_rsci_idat,
            operator_11_false_io_read_widthIn_rsc_cse_sva, or_tmp_57);
        acc_tmp_vinit_ndx_sva <= MUX_v_19_2_2(for_and_nl, (z_out_5(18 DOWNTO 0)),
            or_568_nl);
        HACC_acc_10_psp <= MUX1HOT_v_13_3_2((z_out_2(12 DOWNTO 0)), HACC_acc_10_psp,
            ('0' & HACC_mux1h_8_nl), STD_LOGIC_VECTOR'( (fsm_output(4)) & or_tmp_77
            & or_163_nl));
        HACC_idx_acc_8_psp <= MUX_v_15_2_2((STD_LOGIC_VECTOR'( "00000") & HACC_mux_1_nl),
            (z_out_2(14 DOWNTO 0)), fsm_output(21));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
            <= MUX1HOT_v_3_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1),3)),
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm(34
            DOWNTO 32)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_379_cse & (fsm_output(13))));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
            <= MUX1HOT_v_3_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            1),3)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30,
            (z_out_5(32 DOWNTO 30)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306
            & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29,
            (z_out_5(29)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
            <= MUX1HOT_v_2_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            1),2)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27,
            (z_out_5(28 DOWNTO 27)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306
            & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26,
            (z_out_5(26)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25,
            (z_out_5(25)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
            <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24,
            (z_out_5(24)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23,
            (z_out_5(23)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22,
            (z_out_5(22)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
            <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21,
            (z_out_5(21)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20,
            (z_out_5(20)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19,
            (z_out_5(19)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18,
            (z_out_5(18)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
            <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17,
            (z_out_5(17)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16,
            (z_out_5(16)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
            <= MUX1HOT_v_2_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            1),2)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14,
            (z_out_5(15 DOWNTO 14)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306
            & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13,
            (z_out_5(13)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12,
            (z_out_5(12)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11,
            (z_out_5(11)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
            <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10,
            (z_out_5(10)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9,
            (z_out_5(9)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8,
            (z_out_5(8)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
            <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7,
            (z_out_5(7)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6,
            (z_out_5(6)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5,
            (z_out_5(5)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
            <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4,
            (z_out_5(4)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
            <= MUX1HOT_s_1_3_2((z_out_3(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3,
            (z_out_5(3)), STD_LOGIC_VECTOR'( (fsm_output(7)) & or_tmp_306 & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
            <= MUX1HOT_v_2_4_2(STD_LOGIC_VECTOR'( "01"), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(z_out_6_2(0),
            1),2)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1,
            (z_out_5(2 DOWNTO 1)), STD_LOGIC_VECTOR'( or_462_nl & or_tmp_167 & or_tmp_306
            & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
            <= T_LINE_if_aelse_mux_nl AND (NOT((NOT((fsm_output(9)) OR (fsm_output(12))
            OR (fsm_output(11)))) AND and_dcpl_32));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
            <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_mux_1_nl,
            not_224_nl);
        HACC_mul_2_itm <= MUX1HOT_v_39_3_2((STD_LOGIC_VECTOR'( "000000") & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_nl),
            (STD_LOGIC_VECTOR'( "0000000") & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_1_nl),
            z_out_9, STD_LOGIC_VECTOR'( or_tmp_404 & or_484_nl & (fsm_output(16))));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm
            <= MUX1HOT_v_35_3_2((STD_LOGIC_VECTOR'( "000") & and_nl), (z_out_5(34
            DOWNTO 0)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm,
            STD_LOGIC_VECTOR'( or_486_nl & (fsm_output(10)) & or_dcpl_60));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
            <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl(3);
        ac_math_atan_pi_2mi_return_69_38_sva <= MUX_v_32_2_2(data_out_out, ac_math_atan_pi_2mi_return_69_38_sva,
            fsm_output(9));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
            <= MUX1HOT_v_33_3_2(z_out_8, (z_out_5(32 DOWNTO 0)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva,
            STD_LOGIC_VECTOR'( (fsm_output(9)) & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
            & or_506_nl));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_rsci_idat <= STD_LOGIC_VECTOR'( "0000000000000000");
      ELSIF ( (core_wen AND (fsm_output(29))) = '1' ) THEN
        acc_rsci_idat <= acc_tmp_rsci_data_out_d_oreg;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HROW_y_sva <= STD_LOGIC_VECTOR'( "0000000000");
      ELSIF ( (core_wen AND ((fsm_output(2)) OR (fsm_output(27)))) = '1' ) THEN
        HROW_y_sva <= MUX_v_10_2_2(STD_LOGIC_VECTOR'("0000000000"), (z_out_4(9 DOWNTO
            0)), (fsm_output(27)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_t_7_0_sva <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( (core_wen AND HACC_t_or_cse) = '1' ) THEN
        HACC_t_7_0_sva <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), HACC_t_7_0_sva_1,
            (fsm_output(25)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11
            <= STD_LOGIC_VECTOR'( "000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5
            <= STD_LOGIC_VECTOR'( "00");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32
            <= STD_LOGIC_VECTOR'( "000");
      ELSIF ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            1),3)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32
            DOWNTO 30)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(29)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(28
            DOWNTO 27)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(26)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(25)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(24)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(23)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(22)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(21)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(20)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(19)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(18)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(17)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(16)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(15
            DOWNTO 14)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(13)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(12)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(11)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(10)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(9)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(8)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(7)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(6)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(5)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(4)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
            <= MUX_s_1_2_2((z_out_3(18)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(3)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(0))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
            DOWNTO 30)), (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(29)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(28
            DOWNTO 27)), (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(26
            DOWNTO 25)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(24))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(23
            DOWNTO 22)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(21))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),3)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(20
            DOWNTO 18)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(17))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(16)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(15
            DOWNTO 14)), (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),3)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(13
            DOWNTO 11)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(10))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(9
            DOWNTO 8)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(7))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(6
            DOWNTO 5)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(4))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(3)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(2))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(1)),
            fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0
            <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(0))
            AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (HACC_mul_3_itm(32 DOWNTO 30)),
            (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (HACC_mul_3_itm(29)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (HACC_mul_3_itm(28 DOWNTO 27)),
            (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (HACC_mul_3_itm(26 DOWNTO 25)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
            <= (HACC_mul_3_itm(24)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (HACC_mul_3_itm(23 DOWNTO 22)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
            <= (HACC_mul_3_itm(21)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),3)), (HACC_mul_3_itm(20 DOWNTO 18)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
            <= (HACC_mul_3_itm(17)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (HACC_mul_3_itm(16)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (HACC_mul_3_itm(15 DOWNTO 14)),
            (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),3)), (HACC_mul_3_itm(13 DOWNTO 11)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
            <= (HACC_mul_3_itm(10)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (HACC_mul_3_itm(9 DOWNTO 8)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7
            <= (HACC_mul_3_itm(7)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5
            <= MUX_v_2_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            1),2)), (HACC_mul_3_itm(6 DOWNTO 5)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4
            <= (HACC_mul_3_itm(4)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (HACC_mul_3_itm(3)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
            <= (HACC_mul_3_itm(2)) AND (fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
            (HACC_mul_3_itm(1)), fsm_output(13));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
            <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1,
            (fsm_output(13)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32
            <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1),3)),
            (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm(34
            DOWNTO 32)), fsm_output(13));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1
            <= STD_LOGIC_VECTOR'( "00");
      ELSIF ( (core_wen AND (and_435_cse OR or_tmp_167 OR (fsm_output(13)))) = '1'
          ) THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1
            <= MUX1HOT_v_2_3_2(STD_LOGIC_VECTOR'( "01"), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(z_out_6_2(0),
            1),2)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(2
            DOWNTO 1)), STD_LOGIC_VECTOR'( and_435_cse & or_tmp_167 & (fsm_output(13))));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_slc_HACC_acc_6_itm <= '0';
      ELSIF ( (core_wen AND ((fsm_output(7)) OR (fsm_output(29)) OR (fsm_output(15))
          OR (fsm_output(13)))) = '1' ) THEN
        HACC_slc_HACC_acc_6_itm <= T_LINE_if_aelse_mux1h_17_nl AND (NOT (fsm_output(7)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
            <= z_out_1(4 DOWNTO 0);
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
            <= z_out_3(4 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_idx_HACC_idx_acc_conv_2f_and_itm <= '0';
      ELSIF ( (core_wen AND ((fsm_output(18)) OR (fsm_output(17)) OR (fsm_output(8))))
          = '1' ) THEN
        HACC_idx_HACC_idx_acc_conv_2f_and_itm <= MUX1HOT_s_1_3_2((z_out_5(35)), HACC_idx_HACC_idx_acc_conv_2f_or_nl,
            HACC_idx_HACC_idx_acc_conv_2f_and_nl, STD_LOGIC_VECTOR'( (fsm_output(8))
            & (fsm_output(17)) & (fsm_output(18))));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_mul_3_itm <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000000");
      ELSIF ( (core_wen AND ((fsm_output(15)) OR or_tmp_306)) = '1' ) THEN
        HACC_mul_3_itm <= MUX_v_38_2_2((STD_LOGIC_VECTOR'( "00000") & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_y_2mi_mux1h_nl),
            (z_out_9(37 DOWNTO 0)), fsm_output(15));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm
            <= '0';
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
            <= '0';
      ELSIF ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm
            <= NOT(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
            OR (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl(3)));
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
            <= z_out_5(35);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT((fsm_output(12)) OR (fsm_output(10)) OR or_dcpl_73)))
          = '1' ) THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
            <= MUX_v_33_2_2(z_out_4, (z_out_6(32 DOWNTO 0)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT or_tmp_404)) = '1' ) THEN
        ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
            <= z_out_6;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_t_7_0_sva_1 <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( (core_wen AND (fsm_output(14))) = '1' ) THEN
        HACC_t_7_0_sva_1 <= z_out_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_HACC_slc_HACC_acc_7_39_14_psp_ftd <= STD_LOGIC_VECTOR'( "0000000000000000");
      ELSIF ( (core_wen AND (fsm_output(17))) = '1' ) THEN
        reg_HACC_slc_HACC_acc_7_39_14_psp_ftd <= z_out_5(39 DOWNTO 24);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_HACC_idx_acc_3_psp_ftd <= '0';
        reg_HACC_idx_acc_3_psp_ftd_1 <= STD_LOGIC_VECTOR'( "00");
        reg_HACC_idx_acc_3_psp_ftd_2 <= STD_LOGIC_VECTOR'( "000");
        reg_HACC_idx_acc_3_psp_ftd_3 <= STD_LOGIC_VECTOR'( "00000000000");
      ELSIF ( HACC_idx_and_ssc_2 = '1' ) THEN
        reg_HACC_idx_acc_3_psp_ftd <= z_out_6(16);
        reg_HACC_idx_acc_3_psp_ftd_1 <= MUX1HOT_v_2_3_2((z_out_2(15 DOWNTO 14)),
            reg_HACC_idx_acc_3_psp_ftd_1, (z_out_6(15 DOWNTO 14)), STD_LOGIC_VECTOR'(
            HACC_idx_and_ssc & or_dcpl_54 & (fsm_output(22))));
        reg_HACC_idx_acc_3_psp_ftd_2 <= MUX1HOT_v_3_4_2(('0' & HACC_mux_nl), (z_out_2(13
            DOWNTO 11)), reg_HACC_idx_acc_3_psp_ftd_2, (z_out_6(13 DOWNTO 11)), STD_LOGIC_VECTOR'(
            or_tmp_77 & HACC_idx_or_1_nl & or_dcpl_54 & (fsm_output(22))));
        reg_HACC_idx_acc_3_psp_ftd_3 <= MUX1HOT_v_11_3_2((z_out_2(10 DOWNTO 0)),
            reg_HACC_idx_acc_3_psp_ftd_3, (z_out_6(10 DOWNTO 0)), STD_LOGIC_VECTOR'(
            HACC_idx_or_2_nl & HACC_idx_or_3_nl & (fsm_output(22))));
      END IF;
    END IF;
  END PROCESS;
  HCOL_x_mux_nl <= MUX_v_11_2_2((acc_tmp_vinit_ndx_sva(10 DOWNTO 0)), reg_HACC_idx_acc_3_psp_ftd_3,
      fsm_output(26));
  HCOL_x_nor_nl <= NOT(or_dcpl_22 OR (fsm_output(29)) OR (fsm_output(30)) OR (fsm_output(2))
      OR (fsm_output(27)));
  HCOL_x_HCOL_x_and_nl <= MUX_v_11_2_2(STD_LOGIC_VECTOR'("00000000000"), HCOL_x_mux_nl,
      HCOL_x_nor_nl);
  or_140_nl <= (NOT((NOT and_dcpl_13) OR (fsm_output(28)) OR (fsm_output(29)) OR
      (fsm_output(1)) OR (fsm_output(30)) OR (fsm_output(2)) OR (fsm_output(27))))
      OR and_137_cse OR and_195_cse;
  or_143_nl <= CONV_SL_1_1(fsm_output(30 DOWNTO 29)/=STD_LOGIC_VECTOR'("00"));
  for_mux1h_6_nl <= MUX1HOT_v_19_3_2(STD_LOGIC_VECTOR'( "1100001101001111111"), (STD_LOGIC_VECTOR'(
      "00000000") & HCOL_x_HCOL_x_and_nl), acc_tmp_vinit_ndx_sva, STD_LOGIC_VECTOR'(
      (fsm_output(1)) & or_140_nl & or_143_nl));
  for_nor_nl <= NOT(or_dcpl_22 OR and_186_cse);
  for_and_nl <= MUX_v_19_2_2(STD_LOGIC_VECTOR'("0000000000000000000"), for_mux1h_6_nl,
      for_nor_nl);
  or_568_nl <= and_198_cse OR (fsm_output(28));
  HACC_acc_5_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(acc_tmp_vinit_ndx_sva(10
      DOWNTO 0)), 11), 12) + CONV_SIGNED(CONV_SIGNED(SIGNED('1' & (NOT (operator_11_false_io_read_widthIn_rsc_cse_sva(10
      DOWNTO 1)))), 11), 12) + SIGNED'( "000000000001"), 12));
  or_516_nl <= (fsm_output(21)) OR (fsm_output(15));
  HACC_mux1h_8_nl <= MUX1HOT_v_12_3_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_acc_5_nl),
      12)), (HACC_acc_10_psp(11 DOWNTO 0)), (z_out_2(11 DOWNTO 0)), STD_LOGIC_VECTOR'(
      (fsm_output(14)) & or_516_nl & (fsm_output(20))));
  or_163_nl <= or_dcpl_54 OR CONV_SL_1_1(fsm_output(15 DOWNTO 14)/=STD_LOGIC_VECTOR'("00"));
  HACC_acc_12_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED((reg_HACC_idx_acc_3_psp_ftd_2(1
      DOWNTO 0)) & (reg_HACC_idx_acc_3_psp_ftd_3(10 DOWNTO 4))), 9), 10) + UNSIGNED((HACC_t_7_0_sva(6
      DOWNTO 0)) & STD_LOGIC_VECTOR'( "111")), 10));
  or_165_nl <= or_dcpl_61 OR or_dcpl_60 OR or_dcpl_58 OR (fsm_output(13));
  HACC_mux_1_nl <= MUX_v_10_2_2(STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HACC_acc_12_nl),
      10)), (HACC_idx_acc_8_psp(9 DOWNTO 0)), or_165_nl);
  or_462_nl <= (NOT((fsm_output(7)) OR (fsm_output(9)) OR (fsm_output(12)) OR (fsm_output(11))
      OR (NOT and_dcpl_32))) OR and_435_cse;
  T_LINE_if_aelse_mux_nl <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0,
      (z_out_5(0)), fsm_output(13));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_mux_1_nl
      <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1,
      fsm_output(13));
  not_224_nl <= NOT or_tmp_302;
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_nl
      <= MUX_v_33_2_2(z_out_8, (HACC_mul_2_itm(32 DOWNTO 0)), or_dcpl_60);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_1_nl
      <= MUX_v_32_2_2((HACC_mul_2_itm(31 DOWNTO 0)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm(31
      DOWNTO 0)), fsm_output(13));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_1_nl
      <= MUX_v_32_2_2(STD_LOGIC_VECTOR'("00000000000000000000000000000000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_1_nl,
      or_cse);
  or_484_nl <= (fsm_output(7)) OR (fsm_output(8)) OR (fsm_output(13));
  not_226_nl <= NOT or_tmp_302;
  and_nl <= MUX_v_32_2_2(STD_LOGIC_VECTOR'("00000000000000000000000000000000"), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm(31
      DOWNTO 0)), not_226_nl);
  or_486_nl <= or_dcpl_61 OR or_cse;
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl <=
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & (z_out_3(4 DOWNTO 2))) + SIGNED'(
      "0001"), 4));
  or_506_nl <= or_dcpl_33 OR (fsm_output(15)) OR or_dcpl_73;
  T_LINE_if_aelse_mux1h_17_nl <= MUX1HOT_s_1_3_2((HACC_mul_3_itm(0)), (z_out_6_2(4)),
      (z_out_3(12)), STD_LOGIC_VECTOR'( (fsm_output(13)) & (fsm_output(15)) & (fsm_output(29))));
  HACC_idx_HACC_idx_acc_conv_2f_or_nl <= CONV_SL_1_1(z_out_5(23 DOWNTO 14)/=STD_LOGIC_VECTOR'("0000000000"));
  HACC_idx_HACC_idx_acc_conv_2f_and_nl <= HACC_idx_HACC_idx_acc_conv_2f_and_itm AND
      (z_out_2(13));
  or_497_nl <= or_dcpl_35 OR (fsm_output(10));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_y_2mi_mux1h_nl
      <= MUX1HOT_v_33_4_2(z_out_8, (HACC_mul_3_itm(32 DOWNTO 0)), z_out_7, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4,
      STD_LOGIC_VECTOR'( (fsm_output(8)) & or_497_nl & or_tmp_419 & or_tmp_420));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl
      <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1(4
      DOWNTO 2))) + SIGNED'( "0001"), 4));
  HACC_mux_nl <= MUX_v_2_2_2((z_out_2(12 DOWNTO 11)), (reg_HACC_idx_acc_3_psp_ftd_2(1
      DOWNTO 0)), or_164_ssc);
  HACC_idx_or_1_nl <= (fsm_output(18)) OR HACC_idx_and_ssc;
  HACC_idx_or_2_nl <= HACC_idx_acc_3_psp_sva_mx0c0 OR (fsm_output(18)) OR ((NOT or_164_ssc)
      AND or_tmp_77) OR HACC_idx_and_ssc;
  HACC_idx_or_3_nl <= or_164_ssc OR or_dcpl_54;
  HACC_mux_7_nl <= MUX_v_7_2_2(STD_LOGIC_VECTOR'( "1010011"), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_idx_acc_8_psp(9
      DOWNTO 8)),7)), fsm_output(7));
  HACC_mux_8_nl <= MUX_v_6_2_2((HACC_t_7_0_sva_1(7 DOWNTO 2)), STD_LOGIC_VECTOR'(
      "000001"), fsm_output(7));
  HACC_acc_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HACC_mux_7_nl) + CONV_UNSIGNED(UNSIGNED(HACC_mux_8_nl),
      7), 7));
  z_out_6_2 <= HACC_acc_nl(6 DOWNTO 2);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_3_nl
      <= MUX_v_8_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva),8)),
      HACC_t_7_0_sva, fsm_output(14));
  z_out_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_3_nl)
      + UNSIGNED'( "00000001"), 8));
  HCOL_mux1h_4_nl <= MUX1HOT_v_4_4_2((reg_HACC_idx_acc_3_psp_ftd_2 & (reg_HACC_idx_acc_3_psp_ftd_3(10))),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((NOT (reg_HACC_idx_acc_3_psp_ftd_1(0)))
      & (NOT (reg_HACC_idx_acc_3_psp_ftd_2(2 DOWNTO 1)))),4)), (STD_LOGIC_VECTOR'(
      "00") & (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd(15 DOWNTO 14))), (acc_tmp_rsci_data_out_d_oreg(15
      DOWNTO 12)), STD_LOGIC_VECTOR'( (fsm_output(19)) & (fsm_output(21)) & (fsm_output(18))
      & (fsm_output(24))));
  HCOL_nor_3_nl <= NOT(HACC_t_or_cse OR (fsm_output(14)) OR (fsm_output(5)) OR (fsm_output(4))
      OR (fsm_output(20)) OR (fsm_output(26)));
  HCOL_and_2_nl <= MUX_v_4_2_2(STD_LOGIC_VECTOR'("0000"), HCOL_mux1h_4_nl, HCOL_nor_3_nl);
  HCOL_mux1h_5_nl <= MUX1HOT_s_1_6_2((HACC_t_7_0_sva(7)), (reg_HACC_idx_acc_3_psp_ftd_3(9)),
      (reg_HACC_idx_acc_3_psp_ftd_1(1)), (NOT (reg_HACC_idx_acc_3_psp_ftd_2(0))),
      (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd(13)), (acc_tmp_rsci_data_out_d_oreg(11)),
      STD_LOGIC_VECTOR'( (fsm_output(5)) & (fsm_output(19)) & (fsm_output(20)) &
      (fsm_output(21)) & (fsm_output(18)) & (fsm_output(24))));
  HCOL_or_4_nl <= (HCOL_mux1h_5_nl AND (NOT(HACC_t_or_cse OR (fsm_output(14)) OR
      (fsm_output(26))))) OR (fsm_output(4));
  HCOL_mux1h_6_nl <= MUX1HOT_v_11_10_2((acc_tmp_vinit_ndx_sva(10 DOWNTO 0)), ('0'
      & HROW_y_sva), ((HACC_t_7_0_sva(6 DOWNTO 0)) & STD_LOGIC_VECTOR'( "0001")),
      ((reg_HACC_idx_acc_3_psp_ftd_3(8 DOWNTO 0)) & (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd(1
      DOWNTO 0))), (STD_LOGIC_VECTOR'( "000") & (NOT HACC_t_7_0_sva)), ((reg_HACC_idx_acc_3_psp_ftd_1(0))
      & reg_HACC_idx_acc_3_psp_ftd_2 & (reg_HACC_idx_acc_3_psp_ftd_3(10 DOWNTO 4))),
      (NOT reg_HACC_idx_acc_3_psp_ftd_3), (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd(12
      DOWNTO 2)), operator_11_false_io_read_widthIn_rsc_cse_sva, (acc_tmp_rsci_data_out_d_oreg(10
      DOWNTO 0)), STD_LOGIC_VECTOR'( HACC_t_or_cse & (fsm_output(14)) & (fsm_output(5))
      & (fsm_output(19)) & (fsm_output(4)) & (fsm_output(20)) & (fsm_output(21))
      & (fsm_output(18)) & (fsm_output(26)) & (fsm_output(24))));
  HCOL_or_5_nl <= (NOT(HACC_t_or_cse OR (fsm_output(5)) OR (fsm_output(19)) OR (fsm_output(4))
      OR (fsm_output(20)) OR (fsm_output(21)) OR (fsm_output(18)) OR (fsm_output(26))
      OR (fsm_output(24)))) OR (fsm_output(14));
  HCOL_or_6_nl <= HACC_t_or_cse OR (fsm_output(20)) OR (fsm_output(24));
  HCOL_mux1h_7_nl <= MUX1HOT_v_15_7_2(STD_LOGIC_VECTOR'( "111111111111110"), (STD_LOGIC_VECTOR'(
      "000000") & (operator_10_false_io_read_heightIn_rsc_cse_sva(9 DOWNTO 1))),
      (STD_LOGIC_VECTOR'( "000000") & (NOT (HACC_acc_10_psp(12 DOWNTO 4)))), (STD_LOGIC_VECTOR'(
      "11111111111111") & (NOT HACC_idx_HACC_idx_acc_conv_2f_and_itm)), (STD_LOGIC_VECTOR'(
      "111") & HACC_t_7_0_sva & STD_LOGIC_VECTOR'( "1110")), ((reg_HACC_idx_acc_3_psp_ftd_2(1
      DOWNTO 0)) & reg_HACC_idx_acc_3_psp_ftd_3 & STD_LOGIC_VECTOR'( "10")), STD_LOGIC_VECTOR'(
      "111111100011010"), STD_LOGIC_VECTOR'( HCOL_or_6_nl & (fsm_output(14)) & (fsm_output(5))
      & (fsm_output(19)) & (fsm_output(4)) & (fsm_output(21)) & (fsm_output(18))));
  HCOL_not_6_nl <= NOT (fsm_output(26));
  HCOL_HCOL_nand_1_nl <= NOT(MUX_v_15_2_2(STD_LOGIC_VECTOR'("000000000000000"), HCOL_mux1h_7_nl,
      HCOL_not_6_nl));
  acc_2_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HCOL_and_2_nl & HCOL_or_4_nl
      & HCOL_mux1h_6_nl & HCOL_or_5_nl) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(HCOL_HCOL_nand_1_nl
      & '1'), 16), 17), 17));
  z_out_2 <= acc_2_nl(16 DOWNTO 1);
  for_1_mux1h_5_nl <= MUX1HOT_v_18_4_2((STD_LOGIC_VECTOR'( "000000") & (acc_tmp_vinit_ndx_sva(18
      DOWNTO 7))), ((NOT (HACC_idx_acc_8_psp(9 DOWNTO 0))) & (NOT (reg_HACC_idx_acc_3_psp_ftd_3(3
      DOWNTO 0))) & (NOT (HACC_acc_10_psp(3 DOWNTO 0)))), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva),18)),
      (STD_LOGIC_VECTOR'( "0000000001") & (NOT data_in_rsci_idat_mxwt)), STD_LOGIC_VECTOR'(
      (fsm_output(29)) & (fsm_output(7)) & (fsm_output(8)) & (fsm_output(3))));
  for_1_for_1_or_2_nl <= (NOT((fsm_output(8)) OR (fsm_output(3)))) OR (fsm_output(29));
  for_1_for_1_and_2_nl <= (NOT (fsm_output(3))) AND for_1_nor_2_seb;
  for_1_for_1_and_3_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(for_1_nor_2_seb,
      1),2)), (fsm_output(3)));
  for_1_for_1_or_3_nl <= (fsm_output(29)) OR (fsm_output(3));
  z_out_3 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(SIGNED(for_1_mux1h_5_nl),
      19) + CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(for_1_for_1_or_2_nl & STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(fsm_output(29),
      1),4)) & STD_LOGIC_VECTOR'( "00") & STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(for_1_for_1_and_2_nl
      & STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(for_1_nor_2_seb, 1),2)) & for_1_for_1_and_3_nl
      & for_1_nor_2_seb),7)) & '0' & for_1_for_1_or_3_nl & '1'), 17), 19), 19));
  HROW_HROW_and_13_nl <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30,
      (fsm_output(9)));
  HROW_HROW_and_14_nl <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
      AND (fsm_output(9));
  HROW_HROW_and_15_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27,
      (fsm_output(9)));
  HROW_HROW_and_16_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25,
      (fsm_output(9)));
  HROW_HROW_and_17_nl <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
      AND (fsm_output(9));
  HROW_HROW_and_18_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22,
      (fsm_output(9)));
  HROW_HROW_and_19_nl <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
      AND (fsm_output(9));
  HROW_HROW_and_20_nl <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18,
      (fsm_output(9)));
  HROW_HROW_and_21_nl <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
      AND (fsm_output(9));
  HROW_HROW_and_22_nl <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
      AND (fsm_output(9));
  HROW_HROW_and_23_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14,
      (fsm_output(9)));
  HROW_HROW_and_24_nl <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11,
      (fsm_output(9)));
  HROW_HROW_and_25_nl <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
      AND (fsm_output(9));
  HROW_mux_9_nl <= MUX_v_2_2_2((HROW_y_sva(9 DOWNTO 8)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8,
      fsm_output(9));
  HROW_mux_10_nl <= MUX_s_1_2_2((HROW_y_sva(7)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7,
      fsm_output(9));
  HROW_mux_11_nl <= MUX_v_2_2_2((HROW_y_sva(6 DOWNTO 5)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5,
      fsm_output(9));
  HROW_mux_12_nl <= MUX_s_1_2_2((HROW_y_sva(4)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4,
      fsm_output(9));
  HROW_mux_13_nl <= MUX_s_1_2_2((HROW_y_sva(3)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3,
      fsm_output(9));
  HROW_mux_14_nl <= MUX_s_1_2_2((HROW_y_sva(2)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2,
      fsm_output(9));
  HROW_mux_15_nl <= MUX_s_1_2_2((HROW_y_sva(1)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1,
      fsm_output(9));
  HROW_mux_16_nl <= MUX_s_1_2_2((HROW_y_sva(0)), HACC_slc_HACC_acc_6_itm, fsm_output(9));
  HROW_or_1_nl <= (NOT (fsm_output(27))) OR (fsm_output(9));
  HROW_mux_17_nl <= MUX_v_33_2_2(STD_LOGIC_VECTOR'( "000000000000000000000000000000001"),
      (NOT (HACC_mul_3_itm(32 DOWNTO 0))), fsm_output(9));
  acc_4_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HROW_HROW_and_13_nl & HROW_HROW_and_14_nl
      & HROW_HROW_and_15_nl & HROW_HROW_and_16_nl & HROW_HROW_and_17_nl & HROW_HROW_and_18_nl
      & HROW_HROW_and_19_nl & HROW_HROW_and_20_nl & HROW_HROW_and_21_nl & HROW_HROW_and_22_nl
      & HROW_HROW_and_23_nl & HROW_HROW_and_24_nl & HROW_HROW_and_25_nl & HROW_mux_9_nl
      & HROW_mux_10_nl & HROW_mux_11_nl & HROW_mux_12_nl & HROW_mux_13_nl & HROW_mux_14_nl
      & HROW_mux_15_nl & HROW_mux_16_nl & HROW_or_1_nl) + UNSIGNED(HROW_mux_17_nl
      & '1'), 34));
  z_out_4 <= acc_4_nl(33 DOWNTO 1);
  for_1_or_8_nl <= (fsm_output(28)) OR (fsm_output(2));
  for_1_or_9_nl <= or_tmp_463 OR or_tmp_464;
  for_1_mux1h_6_nl <= MUX1HOT_v_39_7_2((STD_LOGIC_VECTOR'( "00000000000000000000")
      & acc_tmp_vinit_ndx_sva), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
      & (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm(31
      DOWNTO 0))),39)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((NOT ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32)
      & (NOT (HACC_mul_2_itm(31 DOWNTO 0)))),39)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0),39)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((NOT ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32)
      & (NOT (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm(31
      DOWNTO 0)))),39)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0),39)),
      HACC_mul_2_itm, STD_LOGIC_VECTOR'( for_1_or_8_nl & for_1_or_9_nl & (fsm_output(8))
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
      & (fsm_output(9)) & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse
      & (fsm_output(17))));
  for_1_or_10_nl <= (NOT((fsm_output(28)) OR or_tmp_463 OR and_954_cse OR (fsm_output(2))
      OR ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      OR (fsm_output(17)))) OR or_tmp_464 OR (fsm_output(8)) OR and_956_cse OR (fsm_output(9))
      OR ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse;
  for_1_mux1h_7_nl <= MUX1HOT_v_22_8_2((STD_LOGIC_VECTOR'( "000000") & (ac_math_atan_pi_2mi_return_69_38_sva(31
      DOWNTO 16))), (STD_LOGIC_VECTOR'( "111111") & (NOT (ac_math_atan_pi_2mi_return_69_38_sva(31
      DOWNTO 16)))), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((HACC_idx_acc_8_psp(9 DOWNTO
      0)) & (reg_HACC_idx_acc_3_psp_ftd_3(3 DOWNTO 0)) & (HACC_acc_10_psp(3 DOWNTO
      0))),22)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
      DOWNTO 16)),22)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(NOT (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
      DOWNTO 16))),22)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(NOT (HACC_mul_2_itm(32
      DOWNTO 16))),22)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_mul_2_itm(32 DOWNTO
      16)),22)), (HACC_mul_3_itm(37 DOWNTO 16)), STD_LOGIC_VECTOR'( or_tmp_463 &
      or_tmp_464 & or_379_cse & and_954_cse & and_956_cse & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      & (fsm_output(17))));
  for_1_not_6_nl <= NOT (fsm_output(28));
  for_1_and_4_nl <= MUX_v_22_2_2(STD_LOGIC_VECTOR'("0000000000000000000000"), for_1_mux1h_7_nl,
      for_1_not_6_nl);
  for_1_or_11_nl <= MUX_v_22_2_2(for_1_and_4_nl, STD_LOGIC_VECTOR'("1111111111111111111111"),
      (fsm_output(2)));
  for_1_mux1h_8_nl <= MUX1HOT_v_16_8_2(STD_LOGIC_VECTOR'( "0000000000000001"), (ac_math_atan_pi_2mi_return_69_38_sva(15
      DOWNTO 0)), (NOT (ac_math_atan_pi_2mi_return_69_38_sva(15 DOWNTO 0))), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(15
      DOWNTO 0)), (NOT (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(15
      DOWNTO 0))), (NOT (HACC_mul_2_itm(15 DOWNTO 0))), (HACC_mul_2_itm(15 DOWNTO
      0)), (HACC_mul_3_itm(15 DOWNTO 0)), STD_LOGIC_VECTOR'( (fsm_output(28)) & or_tmp_463
      & or_tmp_464 & and_954_cse & and_956_cse & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      & (fsm_output(17))));
  for_1_nor_6_nl <= NOT(CONV_SL_1_1(fsm_output(9 DOWNTO 8)/=STD_LOGIC_VECTOR'("00")));
  for_1_and_5_nl <= MUX_v_16_2_2(STD_LOGIC_VECTOR'("0000000000000000"), for_1_mux1h_8_nl,
      for_1_nor_6_nl);
  for_1_or_12_nl <= MUX_v_16_2_2(for_1_and_5_nl, STD_LOGIC_VECTOR'("1111111111111111"),
      (fsm_output(2)));
  acc_5_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED(for_1_mux1h_6_nl
      & for_1_or_10_nl), 40), 41) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(for_1_or_11_nl
      & for_1_or_12_nl & '1'), 39), 41), 41));
  z_out_5 <= acc_5_nl(40 DOWNTO 1);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30(2),
      1),3)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_29_nl
      <= NOT (fsm_output(22));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_29_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl
      <= MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30(1
      DOWNTO 0)), (HACC_mul_2_itm(31 DOWNTO 30)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_30_nl
      <= NOT (fsm_output(22));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_30_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29,
      (HACC_mul_2_itm(29)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_17_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_18_nl
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27,
      (HACC_mul_2_itm(28 DOWNTO 27)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_32_nl
      <= NOT (fsm_output(22));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_18_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_32_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26,
      (HACC_mul_2_itm(26)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_19_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25,
      (HACC_mul_2_itm(25)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_20_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24,
      (HACC_mul_2_itm(24)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_21_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23,
      (HACC_mul_2_itm(23)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_22_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_23_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22,
      (HACC_mul_2_itm(22)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_23_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_24_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21,
      (HACC_mul_2_itm(21)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_24_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_25_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20,
      (HACC_mul_2_itm(20)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_25_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_26_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19,
      (HACC_mul_2_itm(19)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_26_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_26_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_27_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18,
      (HACC_mul_2_itm(18)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_27_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_27_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_28_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17,
      (HACC_mul_2_itm(17)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_28_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_28_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_18_nl
      <= MUX1HOT_v_16_3_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1),
      (HACC_mul_2_itm(16 DOWNTO 1)), (HACC_idx_acc_8_psp & '0'), STD_LOGIC_VECTOR'(
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm
      & (fsm_output(22))));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_29_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0,
      (HACC_mul_2_itm(0)), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_29_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_29_nl
      AND (NOT (fsm_output(22)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_33_nl
      <= (NOT(or_tmp_420 OR or_tmp_475 OR (fsm_output(22)))) OR or_tmp_419 OR or_tmp_476;
  HACC_idx_acc_9_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(reg_HACC_idx_acc_3_psp_ftd_3
      & (HACC_t_7_0_sva(7 DOWNTO 2))) + CONV_SIGNED(CONV_SIGNED(SIGNED((HACC_acc_10_psp(11
      DOWNTO 0)) & (reg_HACC_idx_acc_3_psp_ftd_3(3 DOWNTO 0))), 16), 17), 17));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_19_nl
      <= MUX1HOT_v_33_5_2((NOT z_out_8), z_out_8, ('0' & data_out_out), ('1' & (NOT
      data_out_out)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(SIGNED(HACC_idx_acc_9_nl),
      17),33)), STD_LOGIC_VECTOR'( or_tmp_419 & or_tmp_420 & or_tmp_475 & or_tmp_476
      & (fsm_output(22))));
  acc_6_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_26_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_27_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_28_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_18_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_29_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_33_nl)
      + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_19_nl
      & '1'), 34), 36), 36));
  z_out_6 <= acc_6_nl(35 DOWNTO 1);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_45_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_13_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_45_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_14_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
      AND (NOT (fsm_output(27)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_47_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_47_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_48_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_48_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
      AND (NOT (fsm_output(27)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_50_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_50_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
      AND (NOT (fsm_output(27)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_52_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_52_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
      AND (NOT (fsm_output(27)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
      AND (NOT (fsm_output(27)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_55_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_55_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_56_nl
      <= NOT (fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_56_nl);
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
      AND (NOT (fsm_output(27)));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_9_nl
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(9 DOWNTO 8)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(7)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_11_nl
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(6 DOWNTO 5)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(4)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(3)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(2)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1,
      (operator_10_false_io_read_heightIn_rsc_cse_sva(1)), fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl
      <= MUX_s_1_2_2(HACC_slc_HACC_acc_6_itm, (operator_10_false_io_read_heightIn_rsc_cse_sva(0)),
      fsm_output(27));
  ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_nl
      <= MUX_v_33_2_2((HACC_mul_3_itm(32 DOWNTO 0)), STD_LOGIC_VECTOR'("111111111111111111111111111111111"),
      (fsm_output(27)));
  z_out_7 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_13_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_14_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_9_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_10_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_11_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_12_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_13_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_14_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl)
      + UNSIGNED(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_nl),
      33));
  HACC_mux_9_nl <= MUX_v_12_2_2((HACC_acc_10_psp(11 DOWNTO 0)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(reg_HACC_idx_acc_3_psp_ftd_3),12)),
      fsm_output(15));
  HACC_mux_10_nl <= MUX_v_27_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva(32
      DOWNTO 6)), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4(32
      DOWNTO 6)), fsm_output(15));
  z_out_9 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(SIGNED'( SIGNED(HACC_mux_9_nl) * SIGNED(HACC_mux_10_nl)),
      39));
END v1;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY getMaxLine IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x1_rsc_vld : OUT STD_LOGIC;
    x1_rsc_rdy : IN STD_LOGIC;
    y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y1_rsc_vld : OUT STD_LOGIC;
    y1_rsc_rdy : IN STD_LOGIC;
    x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x2_rsc_vld : OUT STD_LOGIC;
    x2_rsc_rdy : IN STD_LOGIC;
    y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y2_rsc_vld : OUT STD_LOGIC;
    y2_rsc_rdy : IN STD_LOGIC;
    acc_rsc_dat : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsc_vld : IN STD_LOGIC;
    acc_rsc_rdy : OUT STD_LOGIC
  );
END getMaxLine;

ARCHITECTURE v1 OF getMaxLine IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_a : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_b : STD_LOGIC_VECTOR (26 DOWNTO 0);

  COMPONENT getMaxLine_core
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
      x1_rsc_vld : OUT STD_LOGIC;
      x1_rsc_rdy : IN STD_LOGIC;
      y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
      y1_rsc_vld : OUT STD_LOGIC;
      y1_rsc_rdy : IN STD_LOGIC;
      x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
      x2_rsc_vld : OUT STD_LOGIC;
      x2_rsc_rdy : IN STD_LOGIC;
      y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
      y2_rsc_vld : OUT STD_LOGIC;
      y2_rsc_rdy : IN STD_LOGIC;
      acc_rsc_dat : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsc_vld : IN STD_LOGIC;
      acc_rsc_rdy : OUT STD_LOGIC;
      T_LINE_if_if_dividend1_mul_cmp_a : OUT STD_LOGIC_VECTOR (19 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_b : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_inst_x1_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_y1_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_x2_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_y2_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_a : STD_LOGIC_VECTOR
      (19 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_b : STD_LOGIC_VECTOR
      (26 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z : STD_LOGIC_VECTOR
      (43 DOWNTO 0);

BEGIN
  getMaxLine_core_inst : getMaxLine_core
    PORT MAP(
      clk => clk,
      rst => rst,
      x1_rsc_dat => getMaxLine_core_inst_x1_rsc_dat,
      x1_rsc_vld => x1_rsc_vld,
      x1_rsc_rdy => x1_rsc_rdy,
      y1_rsc_dat => getMaxLine_core_inst_y1_rsc_dat,
      y1_rsc_vld => y1_rsc_vld,
      y1_rsc_rdy => y1_rsc_rdy,
      x2_rsc_dat => getMaxLine_core_inst_x2_rsc_dat,
      x2_rsc_vld => x2_rsc_vld,
      x2_rsc_rdy => x2_rsc_rdy,
      y2_rsc_dat => getMaxLine_core_inst_y2_rsc_dat,
      y2_rsc_vld => y2_rsc_vld,
      y2_rsc_rdy => y2_rsc_rdy,
      acc_rsc_dat => getMaxLine_core_inst_acc_rsc_dat,
      acc_rsc_vld => acc_rsc_vld,
      acc_rsc_rdy => acc_rsc_rdy,
      T_LINE_if_if_dividend1_mul_cmp_a => getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_a,
      T_LINE_if_if_dividend1_mul_cmp_b => getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_b,
      T_LINE_if_if_dividend1_mul_cmp_z => getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z
    );
  x1_rsc_dat <= getMaxLine_core_inst_x1_rsc_dat;
  y1_rsc_dat <= getMaxLine_core_inst_y1_rsc_dat;
  x2_rsc_dat <= getMaxLine_core_inst_x2_rsc_dat;
  y2_rsc_dat <= getMaxLine_core_inst_y2_rsc_dat;
  getMaxLine_core_inst_acc_rsc_dat <= acc_rsc_dat;
  T_LINE_if_if_dividend1_mul_cmp_a <= getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_a;
  T_LINE_if_if_dividend1_mul_cmp_b <= getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_b;
  getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z <= STD_LOGIC_VECTOR(CONV_UNSIGNED(SIGNED'(
      SIGNED(T_LINE_if_if_dividend1_mul_cmp_a) * SIGNED(T_LINE_if_if_dividend1_mul_cmp_b)),
      44));

END v1;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY houghTransform IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    data_in_rsc_vld : IN STD_LOGIC;
    data_in_rsc_rdy : OUT STD_LOGIC;
    widthIn_rsc_dat : IN STD_LOGIC_VECTOR (10 DOWNTO 0);
    widthIn_rsc_triosy_lz : OUT STD_LOGIC;
    heightIn_rsc_dat : IN STD_LOGIC_VECTOR (9 DOWNTO 0);
    heightIn_rsc_triosy_lz : OUT STD_LOGIC;
    acc_rsc_dat : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    acc_rsc_vld : OUT STD_LOGIC;
    acc_rsc_rdy : IN STD_LOGIC
  );
END houghTransform;

ARCHITECTURE v1 OF houghTransform IS
  -- Default Constants
  CONSTANT PWR : STD_LOGIC := '1';

  -- Interconnect Declarations
  SIGNAL widthIn_rsci_idat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL heightIn_rsci_idat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_in_d : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_tmp_rsci_addr_d : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL acc_tmp_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_out_d : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsci_en_d : STD_LOGIC;
  SIGNAL acc_tmp_rsc_en : STD_LOGIC;
  SIGNAL acc_tmp_rsc_data_out : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsc_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_addr : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsc_data_in : STD_LOGIC_VECTOR (31 DOWNTO 0);

  SIGNAL widthIn_rsci_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL widthIn_rsci_idat_1 : STD_LOGIC_VECTOR (10 DOWNTO 0);

  SIGNAL heightIn_rsci_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL heightIn_rsci_idat_1 : STD_LOGIC_VECTOR (9 DOWNTO 0);

  SIGNAL acc_tmp_rsc_comp_data_in : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_addr : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_data_out : STD_LOGIC_VECTOR (31 DOWNTO 0);

  COMPONENT houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
    PORT(
      en : OUT STD_LOGIC;
      data_out : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
      we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      addr : OUT STD_LOGIC_VECTOR (37 DOWNTO 0);
      data_in : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
      data_in_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
      addr_d : IN STD_LOGIC_VECTOR (37 DOWNTO 0);
      re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      data_out_d : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
      en_d : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL acc_tmp_rsci_data_out : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsci_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_addr : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_in : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_in_d_1 : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsci_addr_d_1 : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsci_re_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_we_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_out_d_1 : STD_LOGIC_VECTOR (31 DOWNTO 0);

  COMPONENT houghTransform_core
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
      data_in_rsc_vld : IN STD_LOGIC;
      data_in_rsc_rdy : OUT STD_LOGIC;
      widthIn_rsc_triosy_lz : OUT STD_LOGIC;
      heightIn_rsc_triosy_lz : OUT STD_LOGIC;
      acc_rsc_dat : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsc_vld : OUT STD_LOGIC;
      acc_rsc_rdy : IN STD_LOGIC;
      widthIn_rsci_idat : IN STD_LOGIC_VECTOR (10 DOWNTO 0);
      heightIn_rsci_idat : IN STD_LOGIC_VECTOR (9 DOWNTO 0);
      acc_tmp_rsci_data_in_d : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_tmp_rsci_addr_d : OUT STD_LOGIC_VECTOR (18 DOWNTO 0);
      acc_tmp_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      acc_tmp_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      acc_tmp_rsci_data_out_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
      acc_tmp_rsci_en_d : OUT STD_LOGIC
    );
  END COMPONENT;
  SIGNAL houghTransform_core_inst_data_in_rsc_dat : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL houghTransform_core_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL houghTransform_core_inst_widthIn_rsci_idat : STD_LOGIC_VECTOR (10 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_heightIn_rsci_idat : STD_LOGIC_VECTOR (9 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_data_in_d : STD_LOGIC_VECTOR (15 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_addr_d : STD_LOGIC_VECTOR (18 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_data_out_d : STD_LOGIC_VECTOR (31
      DOWNTO 0);

BEGIN
  widthIn_rsci : work.ccs_in_pkg_v1.ccs_in_v1
    GENERIC MAP(
      rscid => 10,
      width => 11
      )
    PORT MAP(
      dat => widthIn_rsci_dat,
      idat => widthIn_rsci_idat_1
    );
  widthIn_rsci_dat <= widthIn_rsc_dat;
  widthIn_rsci_idat <= widthIn_rsci_idat_1;

  heightIn_rsci : work.ccs_in_pkg_v1.ccs_in_v1
    GENERIC MAP(
      rscid => 11,
      width => 10
      )
    PORT MAP(
      dat => heightIn_rsci_dat,
      idat => heightIn_rsci_idat_1
    );
  heightIn_rsci_dat <= heightIn_rsc_dat;
  heightIn_rsci_idat <= heightIn_rsci_idat_1;

  acc_tmp_rsc_comp : work.ram_sync_dualrw_be_pkg.ram_sync_dualRW_be
    GENERIC MAP(
      ram_id => 13,
      words => 400000,
      width => 16,
      addr_width => 19,
      a_reset_active => 0,
      s_reset_active => 1,
      enable_active => 0,
      re_active => 0,
      we_active => 0,
      num_byte_enables => 1,
      clock_edge => 1,
      no_of_RAM_dualRW_readwrite_port => 2
      )
    PORT MAP(
      data_in => acc_tmp_rsc_comp_data_in,
      addr => acc_tmp_rsc_comp_addr,
      re => acc_tmp_rsc_comp_re,
      we => acc_tmp_rsc_comp_we,
      data_out => acc_tmp_rsc_comp_data_out,
      clk => clk,
      a_rst => '1',
      s_rst => rst,
      en => acc_tmp_rsc_en
    );
  acc_tmp_rsc_comp_data_in <= acc_tmp_rsc_data_in;
  acc_tmp_rsc_comp_addr <= acc_tmp_rsc_addr;
  acc_tmp_rsc_comp_re <= acc_tmp_rsc_re;
  acc_tmp_rsc_comp_we <= acc_tmp_rsc_we;
  acc_tmp_rsc_data_out <= acc_tmp_rsc_comp_data_out;

  acc_tmp_rsci : houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
    PORT MAP(
      en => acc_tmp_rsc_en,
      data_out => acc_tmp_rsci_data_out,
      we => acc_tmp_rsci_we,
      re => acc_tmp_rsci_re,
      addr => acc_tmp_rsci_addr,
      data_in => acc_tmp_rsci_data_in,
      data_in_d => acc_tmp_rsci_data_in_d_1,
      addr_d => acc_tmp_rsci_addr_d_1,
      re_d => acc_tmp_rsci_re_d_1,
      we_d => acc_tmp_rsci_we_d_1,
      data_out_d => acc_tmp_rsci_data_out_d_1,
      en_d => acc_tmp_rsci_en_d
    );
  acc_tmp_rsci_data_out <= acc_tmp_rsc_data_out;
  acc_tmp_rsc_we <= acc_tmp_rsci_we;
  acc_tmp_rsc_re <= acc_tmp_rsci_re;
  acc_tmp_rsc_addr <= acc_tmp_rsci_addr;
  acc_tmp_rsc_data_in <= acc_tmp_rsci_data_in;
  acc_tmp_rsci_data_in_d_1 <= STD_LOGIC_VECTOR'( "0000000000000000") & acc_tmp_rsci_data_in_d;
  acc_tmp_rsci_addr_d_1 <= STD_LOGIC_VECTOR'( "0000000000000000000") & acc_tmp_rsci_addr_d;
  acc_tmp_rsci_re_d_1 <= acc_tmp_rsci_re_d;
  acc_tmp_rsci_we_d_1 <= acc_tmp_rsci_we_d;
  acc_tmp_rsci_data_out_d <= acc_tmp_rsci_data_out_d_1;

  houghTransform_core_inst : houghTransform_core
    PORT MAP(
      clk => clk,
      rst => rst,
      data_in_rsc_dat => houghTransform_core_inst_data_in_rsc_dat,
      data_in_rsc_vld => data_in_rsc_vld,
      data_in_rsc_rdy => data_in_rsc_rdy,
      widthIn_rsc_triosy_lz => widthIn_rsc_triosy_lz,
      heightIn_rsc_triosy_lz => heightIn_rsc_triosy_lz,
      acc_rsc_dat => houghTransform_core_inst_acc_rsc_dat,
      acc_rsc_vld => acc_rsc_vld,
      acc_rsc_rdy => acc_rsc_rdy,
      widthIn_rsci_idat => houghTransform_core_inst_widthIn_rsci_idat,
      heightIn_rsci_idat => houghTransform_core_inst_heightIn_rsci_idat,
      acc_tmp_rsci_data_in_d => houghTransform_core_inst_acc_tmp_rsci_data_in_d,
      acc_tmp_rsci_addr_d => houghTransform_core_inst_acc_tmp_rsci_addr_d,
      acc_tmp_rsci_re_d => houghTransform_core_inst_acc_tmp_rsci_re_d,
      acc_tmp_rsci_we_d => houghTransform_core_inst_acc_tmp_rsci_we_d,
      acc_tmp_rsci_data_out_d => houghTransform_core_inst_acc_tmp_rsci_data_out_d,
      acc_tmp_rsci_en_d => acc_tmp_rsci_en_d
    );
  houghTransform_core_inst_data_in_rsc_dat <= data_in_rsc_dat;
  acc_rsc_dat <= houghTransform_core_inst_acc_rsc_dat;
  houghTransform_core_inst_widthIn_rsci_idat <= widthIn_rsci_idat;
  houghTransform_core_inst_heightIn_rsci_idat <= heightIn_rsci_idat;
  acc_tmp_rsci_data_in_d <= houghTransform_core_inst_acc_tmp_rsci_data_in_d;
  acc_tmp_rsci_addr_d <= houghTransform_core_inst_acc_tmp_rsci_addr_d;
  acc_tmp_rsci_re_d <= houghTransform_core_inst_acc_tmp_rsci_re_d;
  acc_tmp_rsci_we_d <= houghTransform_core_inst_acc_tmp_rsci_we_d;
  houghTransform_core_inst_acc_tmp_rsci_data_out_d <= acc_tmp_rsci_data_out_d;

END v1;

-- ------------------------------------------------------------------
--  Design Unit:    Hough_Algorithm_HW_1296_864
-- ------------------------------------------------------------------

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

USE work.ccs_in_wait_pkg_v1.ALL;
USE work.ccs_out_wait_pkg_v1.ALL;
USE work.mgc_io_sync_pkg_v2.ALL;
USE work.mgc_shift_comps_v5.ALL;
USE work.ccs_in_pkg_v1.ALL;
USE work.ram_sync_dualRW_be_pkg.ALL;
USE work.ccs_pipe_pkg_v5.ALL;


ENTITY Hough_Algorithm_HW_1296_864 IS
  PORT(
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    data_in_rsc_vld : IN STD_LOGIC;
    data_in_rsc_rdy : OUT STD_LOGIC;
    widthIn_rsc_dat : IN STD_LOGIC_VECTOR (10 DOWNTO 0);
    widthIn_rsc_triosy_lz : OUT STD_LOGIC;
    heightIn_rsc_dat : IN STD_LOGIC_VECTOR (9 DOWNTO 0);
    heightIn_rsc_triosy_lz : OUT STD_LOGIC;
    x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x1_rsc_vld : OUT STD_LOGIC;
    x1_rsc_rdy : IN STD_LOGIC;
    y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y1_rsc_vld : OUT STD_LOGIC;
    y1_rsc_rdy : IN STD_LOGIC;
    x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
    x2_rsc_vld : OUT STD_LOGIC;
    x2_rsc_rdy : IN STD_LOGIC;
    y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
    y2_rsc_vld : OUT STD_LOGIC;
    y2_rsc_rdy : IN STD_LOGIC
  );
END Hough_Algorithm_HW_1296_864;

ARCHITECTURE v1 OF Hough_Algorithm_HW_1296_864 IS
  -- Default Constants
  CONSTANT PWR : STD_LOGIC := '1';
  CONSTANT GND : STD_LOGIC := '0';

  -- Interconnect Declarations
  SIGNAL acc_rsc_dat_nhoughTransform_inst : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_rsc_rdy_nhoughTransform_inst : STD_LOGIC;
  SIGNAL x1_rsc_dat_ngetMaxLine_inst : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL y1_rsc_dat_ngetMaxLine_inst : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL x2_rsc_dat_ngetMaxLine_inst : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL y2_rsc_dat_ngetMaxLine_inst : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL acc_rsc_dat_ngetMaxLine_inst : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_rsc_vld_ngetMaxLine_inst : STD_LOGIC;
  SIGNAL data_in_rsc_rdy_nhoughTransform_inst_bud : STD_LOGIC;
  SIGNAL widthIn_rsc_triosy_lz_nhoughTransform_inst_bud : STD_LOGIC;
  SIGNAL heightIn_rsc_triosy_lz_nhoughTransform_inst_bud : STD_LOGIC;
  SIGNAL acc_rsc_vld_nhoughTransform_inst_bud : STD_LOGIC;
  SIGNAL acc_rsc_rdy_ngetMaxLine_inst_bud : STD_LOGIC;
  SIGNAL x1_rsc_vld_ngetMaxLine_inst_bud : STD_LOGIC;
  SIGNAL y1_rsc_vld_ngetMaxLine_inst_bud : STD_LOGIC;
  SIGNAL x2_rsc_vld_ngetMaxLine_inst_bud : STD_LOGIC;
  SIGNAL y2_rsc_vld_ngetMaxLine_inst_bud : STD_LOGIC;
  SIGNAL acc_unc_2 : STD_LOGIC;
  SIGNAL acc_idle : STD_LOGIC;

  SIGNAL acc_cns_pipe_din : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_cns_pipe_dout : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_cns_pipe_sz : STD_LOGIC_VECTOR (0 DOWNTO 0);

  COMPONENT houghTransform
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      data_in_rsc_dat : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
      data_in_rsc_vld : IN STD_LOGIC;
      data_in_rsc_rdy : OUT STD_LOGIC;
      widthIn_rsc_dat : IN STD_LOGIC_VECTOR (10 DOWNTO 0);
      widthIn_rsc_triosy_lz : OUT STD_LOGIC;
      heightIn_rsc_dat : IN STD_LOGIC_VECTOR (9 DOWNTO 0);
      heightIn_rsc_triosy_lz : OUT STD_LOGIC;
      acc_rsc_dat : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsc_vld : OUT STD_LOGIC;
      acc_rsc_rdy : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL houghTransform_inst_data_in_rsc_dat : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL houghTransform_inst_widthIn_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL houghTransform_inst_heightIn_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL houghTransform_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);

  COMPONENT getMaxLine
    PORT(
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      x1_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
      x1_rsc_vld : OUT STD_LOGIC;
      x1_rsc_rdy : IN STD_LOGIC;
      y1_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
      y1_rsc_vld : OUT STD_LOGIC;
      y1_rsc_rdy : IN STD_LOGIC;
      x2_rsc_dat : OUT STD_LOGIC_VECTOR (10 DOWNTO 0);
      x2_rsc_vld : OUT STD_LOGIC;
      x2_rsc_rdy : IN STD_LOGIC;
      y2_rsc_dat : OUT STD_LOGIC_VECTOR (9 DOWNTO 0);
      y2_rsc_vld : OUT STD_LOGIC;
      y2_rsc_rdy : IN STD_LOGIC;
      acc_rsc_dat : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      acc_rsc_vld : IN STD_LOGIC;
      acc_rsc_rdy : OUT STD_LOGIC
    );
  END COMPONENT;
  SIGNAL getMaxLine_inst_x1_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_inst_y1_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_inst_x2_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_inst_y2_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);

BEGIN
  acc_cns_pipe : work.ccs_pipe_pkg_v5.ccs_pipe_v5
    GENERIC MAP(
      rscid => 21,
      width => 16,
      sz_width => 1,
      fifo_sz => 1,
      log2_sz => 0,
      ph_clk => 1,
      ph_en => 0,
      ph_arst => 0,
      ph_srst => 1
      )
    PORT MAP(
      clk => clk,
      en => '0',
      arst => '1',
      srst => rst,
      din_rdy => acc_rsc_rdy_nhoughTransform_inst,
      din_vld => acc_rsc_vld_nhoughTransform_inst_bud,
      din => acc_cns_pipe_din,
      dout_rdy => acc_rsc_rdy_ngetMaxLine_inst_bud,
      dout_vld => acc_rsc_vld_ngetMaxLine_inst,
      dout => acc_cns_pipe_dout,
      sz => acc_cns_pipe_sz,
      sz_req => '0',
      is_idle => acc_idle
    );
  acc_cns_pipe_din <= acc_rsc_dat_nhoughTransform_inst;
  acc_rsc_dat_ngetMaxLine_inst <= acc_cns_pipe_dout;
  acc_unc_2 <= acc_cns_pipe_sz(0);

  houghTransform_inst : houghTransform
    PORT MAP(
      clk => clk,
      rst => rst,
      data_in_rsc_dat => houghTransform_inst_data_in_rsc_dat,
      data_in_rsc_vld => data_in_rsc_vld,
      data_in_rsc_rdy => data_in_rsc_rdy_nhoughTransform_inst_bud,
      widthIn_rsc_dat => houghTransform_inst_widthIn_rsc_dat,
      widthIn_rsc_triosy_lz => widthIn_rsc_triosy_lz_nhoughTransform_inst_bud,
      heightIn_rsc_dat => houghTransform_inst_heightIn_rsc_dat,
      heightIn_rsc_triosy_lz => heightIn_rsc_triosy_lz_nhoughTransform_inst_bud,
      acc_rsc_dat => houghTransform_inst_acc_rsc_dat,
      acc_rsc_vld => acc_rsc_vld_nhoughTransform_inst_bud,
      acc_rsc_rdy => acc_rsc_rdy_nhoughTransform_inst
    );
  houghTransform_inst_data_in_rsc_dat <= data_in_rsc_dat;
  houghTransform_inst_widthIn_rsc_dat <= widthIn_rsc_dat;
  houghTransform_inst_heightIn_rsc_dat <= heightIn_rsc_dat;
  acc_rsc_dat_nhoughTransform_inst <= houghTransform_inst_acc_rsc_dat;

  getMaxLine_inst : getMaxLine
    PORT MAP(
      clk => clk,
      rst => rst,
      x1_rsc_dat => getMaxLine_inst_x1_rsc_dat,
      x1_rsc_vld => x1_rsc_vld_ngetMaxLine_inst_bud,
      x1_rsc_rdy => x1_rsc_rdy,
      y1_rsc_dat => getMaxLine_inst_y1_rsc_dat,
      y1_rsc_vld => y1_rsc_vld_ngetMaxLine_inst_bud,
      y1_rsc_rdy => y1_rsc_rdy,
      x2_rsc_dat => getMaxLine_inst_x2_rsc_dat,
      x2_rsc_vld => x2_rsc_vld_ngetMaxLine_inst_bud,
      x2_rsc_rdy => x2_rsc_rdy,
      y2_rsc_dat => getMaxLine_inst_y2_rsc_dat,
      y2_rsc_vld => y2_rsc_vld_ngetMaxLine_inst_bud,
      y2_rsc_rdy => y2_rsc_rdy,
      acc_rsc_dat => getMaxLine_inst_acc_rsc_dat,
      acc_rsc_vld => acc_rsc_vld_ngetMaxLine_inst,
      acc_rsc_rdy => acc_rsc_rdy_ngetMaxLine_inst_bud
    );
  x1_rsc_dat_ngetMaxLine_inst <= getMaxLine_inst_x1_rsc_dat;
  y1_rsc_dat_ngetMaxLine_inst <= getMaxLine_inst_y1_rsc_dat;
  x2_rsc_dat_ngetMaxLine_inst <= getMaxLine_inst_x2_rsc_dat;
  y2_rsc_dat_ngetMaxLine_inst <= getMaxLine_inst_y2_rsc_dat;
  getMaxLine_inst_acc_rsc_dat <= acc_rsc_dat_ngetMaxLine_inst;

  data_in_rsc_rdy <= data_in_rsc_rdy_nhoughTransform_inst_bud;
  widthIn_rsc_triosy_lz <= widthIn_rsc_triosy_lz_nhoughTransform_inst_bud;
  heightIn_rsc_triosy_lz <= heightIn_rsc_triosy_lz_nhoughTransform_inst_bud;
  x1_rsc_vld <= x1_rsc_vld_ngetMaxLine_inst_bud;
  x1_rsc_dat <= x1_rsc_dat_ngetMaxLine_inst;
  y1_rsc_vld <= y1_rsc_vld_ngetMaxLine_inst_bud;
  y1_rsc_dat <= y1_rsc_dat_ngetMaxLine_inst;
  x2_rsc_vld <= x2_rsc_vld_ngetMaxLine_inst_bud;
  x2_rsc_dat <= x2_rsc_dat_ngetMaxLine_inst;
  y2_rsc_vld <= y2_rsc_vld_ngetMaxLine_inst_bud;
  y2_rsc_dat <= y2_rsc_dat_ngetMaxLine_inst;
END v1;



