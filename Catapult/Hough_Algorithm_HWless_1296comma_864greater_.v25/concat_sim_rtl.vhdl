
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


--------> ./rtl_hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60.vhdl 
-- ----------------------------------------------------------------------
--  HLS HDL:        VHDL Netlister
--  HLS Version:    10.5a/871028 Production Release
--  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
-- 
--  Generated by:   user2@edatools.ee.duth.gr
--  Generated date: Wed Jul  7 20:56:14 2021
-- ----------------------------------------------------------------------

-- 
LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


PACKAGE hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60_pkg IS 
  COMPONENT hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
  END COMPONENT;
END hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60_pkg;

PACKAGE BODY hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60_pkg IS
END hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60_pkg;

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


USE work.hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60_pkg.all;
LIBRARY std;

USE std.textio.all;
USE ieee.std_logic_textio.all;

ENTITY hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60 IS
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60;

ARCHITECTURE v25 OF hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60 IS
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

END v25;



--------> ./rtl_hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60.vhdl 
-- ----------------------------------------------------------------------
--  HLS HDL:        VHDL Netlister
--  HLS Version:    10.5a/871028 Production Release
--  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
-- 
--  Generated by:   user2@edatools.ee.duth.gr
--  Generated date: Wed Jul  7 20:56:14 2021
-- ----------------------------------------------------------------------

-- 
LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


PACKAGE hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60_pkg IS 
  COMPONENT hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
  END COMPONENT;
END hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60_pkg;

PACKAGE BODY hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60_pkg IS
END hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60_pkg;

LIBRARY IEEE;

USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;


USE work.hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60_pkg.all;
LIBRARY std;

USE std.textio.all;
USE ieee.std_logic_textio.all;

ENTITY hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60 IS
    PORT (
      addr : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60;

ARCHITECTURE v25 OF hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60 IS
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

END v25;



--------> ./rtl.vhdl 
-- ----------------------------------------------------------------------
--  HLS HDL:        VHDL Netlister
--  HLS Version:    10.5a/871028 Production Release
--  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
-- 
--  Generated by:   user2@edatools.ee.duth.gr
--  Generated date: Wed Jul  7 20:56:14 2021
-- ----------------------------------------------------------------------

-- 
-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
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


ENTITY getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  PORT(
    en : OUT STD_LOGIC;
    data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    en_d : IN STD_LOGIC
  );
END getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen;

ARCHITECTURE v25 OF getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  -- Default Constants

BEGIN
  en <= en_d;
  data_out_d <= data_out;
  we <= we_d;
  re <= re_d;
  addr <= addr_d;
  data_in <= data_in_d;
END v25;

-- ------------------------------------------------------------------
--  Design Unit:    getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
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


ENTITY getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  PORT(
    en : OUT STD_LOGIC;
    data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    en_d : IN STD_LOGIC
  );
END getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen;

ARCHITECTURE v25 OF getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  -- Default Constants

BEGIN
  en <= en_d;
  data_out_d <= data_out;
  we <= we_d;
  re <= re_d;
  addr <= addr_d;
  data_in <= data_in_d;
END v25;

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
    fsm_output : OUT STD_LOGIC_VECTOR (21 DOWNTO 0);
    for_C_0_tr0 : IN STD_LOGIC;
    T_LINE_C_5_tr0 : IN STD_LOGIC;
    T_LINE_C_5_tr1 : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
        : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
        : IN STD_LOGIC;
    T_LINE_C_9_tr0 : IN STD_LOGIC;
    T_LINE_C_9_tr1 : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
        : IN STD_LOGIC;
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
        : IN STD_LOGIC;
    T_LINE_C_13_tr0 : IN STD_LOGIC;
    R_LINE_C_0_tr0 : IN STD_LOGIC
  );
END getMaxLine_core_core_fsm;

ARCHITECTURE v25 OF getMaxLine_core_core_fsm IS
  -- Default Constants

  -- FSM State Type Declaration for getMaxLine_core_core_fsm_1
  TYPE getMaxLine_core_core_fsm_1_ST IS (main_C_0, for_C_0, T_LINE_C_0, T_LINE_C_1,
      T_LINE_C_2, T_LINE_C_3, T_LINE_C_4, T_LINE_C_5, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0,
      T_LINE_C_6, T_LINE_C_7, T_LINE_C_8, T_LINE_C_9, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0,
      T_LINE_C_10, T_LINE_C_11, T_LINE_C_12, T_LINE_C_13, R_LINE_C_0, main_C_1);

  SIGNAL state_var : getMaxLine_core_core_fsm_1_ST;
  SIGNAL state_var_NS : getMaxLine_core_core_fsm_1_ST;

BEGIN
  getMaxLine_core_core_fsm_1 : PROCESS (for_C_0_tr0, T_LINE_C_5_tr0, T_LINE_C_5_tr1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_9_tr0, T_LINE_C_9_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_13_tr0, R_LINE_C_0_tr0, state_var)
  BEGIN
    CASE state_var IS
      WHEN for_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000000000010");
        IF ( for_C_0_tr0 = '1' ) THEN
          state_var_NS <= T_LINE_C_0;
        ELSE
          state_var_NS <= for_C_0;
        END IF;
      WHEN T_LINE_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000000000100");
        state_var_NS <= T_LINE_C_1;
      WHEN T_LINE_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000000001000");
        state_var_NS <= T_LINE_C_2;
      WHEN T_LINE_C_2 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000000010000");
        state_var_NS <= T_LINE_C_3;
      WHEN T_LINE_C_3 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000000100000");
        state_var_NS <= T_LINE_C_4;
      WHEN T_LINE_C_4 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000001000000");
        state_var_NS <= T_LINE_C_5;
      WHEN T_LINE_C_5 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000010000000");
        IF ( T_LINE_C_5_tr0 = '1' ) THEN
          state_var_NS <= T_LINE_C_6;
        ELSIF ( T_LINE_C_5_tr1 = '1' ) THEN
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000100000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_6;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000001000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_6;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        END IF;
      WHEN T_LINE_C_6 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000010000000000");
        state_var_NS <= T_LINE_C_7;
      WHEN T_LINE_C_7 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000100000000000");
        state_var_NS <= T_LINE_C_8;
      WHEN T_LINE_C_8 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000001000000000000");
        state_var_NS <= T_LINE_C_9;
      WHEN T_LINE_C_9 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000010000000000000");
        IF ( T_LINE_C_9_tr0 = '1' ) THEN
          state_var_NS <= T_LINE_C_10;
        ELSIF ( T_LINE_C_9_tr1 = '1' ) THEN
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000100000000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_10;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        END IF;
      WHEN ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
          =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000001000000000000000");
        IF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
            = '1' ) THEN
          state_var_NS <= T_LINE_C_10;
        ELSE
          state_var_NS <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        END IF;
      WHEN T_LINE_C_10 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000010000000000000000");
        state_var_NS <= T_LINE_C_11;
      WHEN T_LINE_C_11 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000100000000000000000");
        state_var_NS <= T_LINE_C_12;
      WHEN T_LINE_C_12 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0001000000000000000000");
        state_var_NS <= T_LINE_C_13;
      WHEN T_LINE_C_13 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0010000000000000000000");
        IF ( T_LINE_C_13_tr0 = '1' ) THEN
          state_var_NS <= R_LINE_C_0;
        ELSE
          state_var_NS <= T_LINE_C_0;
        END IF;
      WHEN R_LINE_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "0100000000000000000000");
        IF ( R_LINE_C_0_tr0 = '1' ) THEN
          state_var_NS <= main_C_1;
        ELSE
          state_var_NS <= T_LINE_C_0;
        END IF;
      WHEN main_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "1000000000000000000000");
        state_var_NS <= main_C_0;
      -- main_C_0
      WHEN OTHERS =>
        fsm_output <= STD_LOGIC_VECTOR'( "0000000000000000000001");
        state_var_NS <= for_C_0;
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

END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_staller IS
  -- Default Constants

BEGIN
  core_wen <= x1_rsci_wen_comp AND y1_rsci_wen_comp AND x2_rsci_wen_comp AND y2_rsci_wen_comp
      AND acc_rsci_wen_comp;
END v25;

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
    cos_out_1_rsc_cgo_iro : IN STD_LOGIC;
    cos_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    cos_out_1_rsci_en_d : OUT STD_LOGIC;
    sin_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0);
    core_wen : IN STD_LOGIC;
    cos_out_1_rsc_cgo : IN STD_LOGIC;
    cos_out_1_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    sin_out_1_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_z_oreg : OUT STD_LOGIC_VECTOR (43 DOWNTO 0)
  );
END getMaxLine_core_wait_dp;

ARCHITECTURE v25 OF getMaxLine_core_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL cos_out_1_rsci_en_d_drv : STD_LOGIC;

  -- Interconnect Declarations
  SIGNAL cos_out_1_rsci_data_out_d_oreg_pconst_26_0 : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL sin_out_1_rsci_data_out_d_oreg_pconst_26_0 : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
      : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  cos_out_1_rsci_en_d <= cos_out_1_rsci_en_d_drv;

  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
      <= NOT(core_wen AND (cos_out_1_rsc_cgo OR cos_out_1_rsc_cgo_iro));
  cos_out_1_rsci_en_d_drv <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse;
  cos_out_1_rsci_data_out_d_oreg <= cos_out_1_rsci_data_out_d_oreg_pconst_26_0;
  sin_out_1_rsci_data_out_d_oreg <= sin_out_1_rsci_data_out_d_oreg_pconst_26_0;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        cos_out_1_rsci_data_out_d_oreg_pconst_26_0 <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( cos_out_1_rsci_en_d_drv = '0' ) THEN
        cos_out_1_rsci_data_out_d_oreg_pconst_26_0 <= cos_out_1_rsci_data_out_d(26
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        sin_out_1_rsci_data_out_d_oreg_pconst_26_0 <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
          = '0' ) THEN
        sin_out_1_rsci_data_out_d_oreg_pconst_26_0 <= sin_out_1_rsci_data_out_d(26
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_acc_rsci_acc_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_acc_rsci_acc_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL acc_rsci_ogwt : STD_LOGIC;

BEGIN
  acc_rsci_bdwt <= acc_rsci_oswt AND core_wen;
  acc_rsci_biwt <= acc_rsci_ogwt AND acc_rsci_ivld;
  acc_rsci_ogwt <= acc_rsci_oswt AND (NOT acc_rsci_bcwt);
  acc_rsci_irdy_core_sct <= acc_rsci_ogwt;
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_y2_rsci_y2_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_y2_rsci_y2_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL y2_rsci_ogwt : STD_LOGIC;

BEGIN
  y2_rsci_bdwt <= y2_rsci_oswt AND core_wen;
  y2_rsci_biwt <= y2_rsci_ogwt AND y2_rsci_irdy;
  y2_rsci_ogwt <= y2_rsci_oswt AND (NOT y2_rsci_bcwt);
  y2_rsci_ivld_core_sct <= y2_rsci_ogwt;
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_x2_rsci_x2_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_x2_rsci_x2_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL x2_rsci_ogwt : STD_LOGIC;

BEGIN
  x2_rsci_bdwt <= x2_rsci_oswt AND core_wen;
  x2_rsci_biwt <= x2_rsci_ogwt AND x2_rsci_irdy;
  x2_rsci_ogwt <= x2_rsci_oswt AND (NOT x2_rsci_bcwt);
  x2_rsci_ivld_core_sct <= x2_rsci_ogwt;
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_y1_rsci_y1_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_y1_rsci_y1_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL y1_rsci_ogwt : STD_LOGIC;

BEGIN
  y1_rsci_bdwt <= y1_rsci_oswt AND core_wen;
  y1_rsci_biwt <= y1_rsci_ogwt AND y1_rsci_irdy;
  y1_rsci_ogwt <= y1_rsci_oswt AND (NOT y1_rsci_bcwt);
  y1_rsci_ivld_core_sct <= y1_rsci_ogwt;
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_x1_rsci_x1_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_x1_rsci_x1_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL x1_rsci_ogwt : STD_LOGIC;

BEGIN
  x1_rsci_bdwt <= x1_rsci_oswt AND core_wen;
  x1_rsci_biwt <= x1_rsci_ogwt AND x1_rsci_irdy;
  x1_rsci_ogwt <= x1_rsci_oswt AND (NOT x1_rsci_bcwt);
  x1_rsci_ivld_core_sct <= x1_rsci_ogwt;
END v25;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
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


ENTITY houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  PORT(
    en : OUT STD_LOGIC;
    data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    en_d : IN STD_LOGIC
  );
END houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen;

ARCHITECTURE v25 OF houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  -- Default Constants

BEGIN
  en <= en_d;
  data_out_d <= data_out;
  we <= we_d;
  re <= re_d;
  addr <= addr_d;
  data_in <= data_in_d;
END v25;

-- ------------------------------------------------------------------
--  Design Unit:    houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
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


ENTITY houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  PORT(
    en : OUT STD_LOGIC;
    data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
    data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
    en_d : IN STD_LOGIC
  );
END houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen;

ARCHITECTURE v25 OF houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    IS
  -- Default Constants

BEGIN
  en <= en_d;
  data_out_d <= data_out;
  we <= we_d;
  re <= re_d;
  addr <= addr_d;
  data_in <= data_in_d;
END v25;

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

ARCHITECTURE v25 OF houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
    IS
  -- Default Constants

BEGIN
  en <= en_d;
  data_out_d <= data_out;
  we <= we_d;
  re <= re_d;
  addr <= addr_d;
  data_in <= data_in_d;
END v25;

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
    fsm_output : OUT STD_LOGIC_VECTOR (11 DOWNTO 0);
    acc_tmp_vinit_C_0_tr0 : IN STD_LOGIC;
    for_1_C_0_tr0 : IN STD_LOGIC;
    HCOL_C_1_tr0 : IN STD_LOGIC;
    HACC_C_0_tr0 : IN STD_LOGIC;
    HCOL_C_2_tr0 : IN STD_LOGIC;
    HROW_C_1_tr0 : IN STD_LOGIC;
    WRITE_C_0_tr0 : IN STD_LOGIC
  );
END houghTransform_core_core_fsm;

ARCHITECTURE v25 OF houghTransform_core_core_fsm IS
  -- Default Constants

  -- FSM State Type Declaration for houghTransform_core_core_fsm_1
  TYPE houghTransform_core_core_fsm_1_ST IS (core_rlp_C_0, main_C_0, acc_tmp_vinit_C_0,
      for_1_C_0, HCOL_C_0, HCOL_C_1, HACC_C_0, HCOL_C_2, HROW_C_0, HROW_C_1, WRITE_C_0,
      main_C_1);

  SIGNAL state_var : houghTransform_core_core_fsm_1_ST;
  SIGNAL state_var_NS : houghTransform_core_core_fsm_1_ST;

BEGIN
  houghTransform_core_core_fsm_1 : PROCESS (acc_tmp_vinit_C_0_tr0, for_1_C_0_tr0,
      HCOL_C_1_tr0, HACC_C_0_tr0, HCOL_C_2_tr0, HROW_C_1_tr0, WRITE_C_0_tr0, state_var)
  BEGIN
    CASE state_var IS
      WHEN main_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000000000010");
        state_var_NS <= acc_tmp_vinit_C_0;
      WHEN acc_tmp_vinit_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000000000100");
        IF ( acc_tmp_vinit_C_0_tr0 = '1' ) THEN
          state_var_NS <= for_1_C_0;
        ELSE
          state_var_NS <= acc_tmp_vinit_C_0;
        END IF;
      WHEN for_1_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000000001000");
        IF ( for_1_C_0_tr0 = '1' ) THEN
          state_var_NS <= HCOL_C_0;
        ELSE
          state_var_NS <= for_1_C_0;
        END IF;
      WHEN HCOL_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000000010000");
        state_var_NS <= HCOL_C_1;
      WHEN HCOL_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000000100000");
        IF ( HCOL_C_1_tr0 = '1' ) THEN
          state_var_NS <= HCOL_C_2;
        ELSE
          state_var_NS <= HACC_C_0;
        END IF;
      WHEN HACC_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000001000000");
        IF ( HACC_C_0_tr0 = '1' ) THEN
          state_var_NS <= HCOL_C_2;
        ELSE
          state_var_NS <= HACC_C_0;
        END IF;
      WHEN HCOL_C_2 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000010000000");
        IF ( HCOL_C_2_tr0 = '1' ) THEN
          state_var_NS <= HROW_C_0;
        ELSE
          state_var_NS <= HCOL_C_0;
        END IF;
      WHEN HROW_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "000100000000");
        state_var_NS <= HROW_C_1;
      WHEN HROW_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "001000000000");
        IF ( HROW_C_1_tr0 = '1' ) THEN
          state_var_NS <= WRITE_C_0;
        ELSE
          state_var_NS <= HCOL_C_0;
        END IF;
      WHEN WRITE_C_0 =>
        fsm_output <= STD_LOGIC_VECTOR'( "010000000000");
        IF ( WRITE_C_0_tr0 = '1' ) THEN
          state_var_NS <= main_C_1;
        ELSE
          state_var_NS <= WRITE_C_0;
        END IF;
      WHEN main_C_1 =>
        fsm_output <= STD_LOGIC_VECTOR'( "100000000000");
        state_var_NS <= main_C_0;
      -- core_rlp_C_0
      WHEN OTHERS =>
        fsm_output <= STD_LOGIC_VECTOR'( "000000000001");
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

END v25;

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

ARCHITECTURE v25 OF houghTransform_core_staller IS
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
    IS
  -- Default Constants

BEGIN
  heightIn_rsc_triosy_obj_ld_core_sct <= heightIn_rsc_triosy_obj_iswt0 AND (NOT core_wten);
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl
    IS
  -- Default Constants

BEGIN
  widthIn_rsc_triosy_obj_ld_core_sct <= widthIn_rsc_triosy_obj_iswt0 AND (NOT core_wten);
END v25;

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
    cos_out_rsc_cgo_iro : IN STD_LOGIC;
    cos_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    cos_out_rsci_en_d : OUT STD_LOGIC;
    sin_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    core_wen : IN STD_LOGIC;
    acc_tmp_rsc_cgo : IN STD_LOGIC;
    acc_tmp_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
    cos_out_rsc_cgo : IN STD_LOGIC;
    cos_out_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    sin_out_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0)
  );
END houghTransform_core_wait_dp;

ARCHITECTURE v25 OF houghTransform_core_wait_dp IS
  -- Default Constants

  -- Output Reader Declarations
  SIGNAL acc_tmp_rsci_en_d_drv : STD_LOGIC;
  SIGNAL cos_out_rsci_en_d_drv : STD_LOGIC;

  -- Interconnect Declarations
  SIGNAL cos_out_rsci_data_out_d_oreg_pconst_26_0 : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL sin_out_rsci_data_out_d_oreg_pconst_26_0 : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
      : STD_LOGIC;

BEGIN
  -- Output Reader Assignments
  acc_tmp_rsci_en_d <= acc_tmp_rsci_en_d_drv;
  cos_out_rsci_en_d <= cos_out_rsci_en_d_drv;

  acc_tmp_rsci_en_d_drv <= NOT(core_wen AND (acc_tmp_rsc_cgo OR acc_tmp_rsc_cgo_iro));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
      <= NOT(core_wen AND (cos_out_rsc_cgo OR cos_out_rsc_cgo_iro));
  cos_out_rsci_en_d_drv <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse;
  cos_out_rsci_data_out_d_oreg <= cos_out_rsci_data_out_d_oreg_pconst_26_0;
  sin_out_rsci_data_out_d_oreg <= sin_out_rsci_data_out_d_oreg_pconst_26_0;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_tmp_rsci_data_out_d_oreg <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( acc_tmp_rsci_en_d_drv = '0' ) THEN
        acc_tmp_rsci_data_out_d_oreg <= acc_tmp_rsci_data_out_d;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        cos_out_rsci_data_out_d_oreg_pconst_26_0 <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( cos_out_rsci_en_d_drv = '0' ) THEN
        cos_out_rsci_data_out_d_oreg_pconst_26_0 <= cos_out_rsci_data_out_d(26 DOWNTO
            0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        sin_out_rsci_data_out_d_oreg_pconst_26_0 <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
          = '0' ) THEN
        sin_out_rsci_data_out_d_oreg_pconst_26_0 <= sin_out_rsci_data_out_d(26 DOWNTO
            0);
      END IF;
    END IF;
  END PROCESS;
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_acc_rsci_acc_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_acc_rsci_acc_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL acc_rsci_ogwt : STD_LOGIC;

BEGIN
  acc_rsci_bdwt <= acc_rsci_oswt AND core_wen;
  acc_rsci_biwt <= acc_rsci_ogwt AND acc_rsci_irdy;
  acc_rsci_ogwt <= acc_rsci_oswt AND (NOT acc_rsci_bcwt);
  acc_rsci_ivld_core_sct <= acc_rsci_ogwt;
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_data_in_rsci_data_in_wait_dp IS
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_data_in_rsci_data_in_wait_ctrl IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL data_in_rsci_ogwt : STD_LOGIC;

BEGIN
  data_in_rsci_bdwt <= data_in_rsci_oswt AND core_wen;
  data_in_rsci_biwt <= data_in_rsci_ogwt AND data_in_rsci_ivld;
  data_in_rsci_ogwt <= data_in_rsci_oswt AND (NOT data_in_rsci_bcwt);
  data_in_rsci_irdy_core_sct <= data_in_rsci_ogwt;
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_acc_rsci IS
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
      rscid => 21,
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

END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_y2_rsci IS
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
      rscid => 20,
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_x2_rsci IS
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
      rscid => 19,
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_y1_rsci IS
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
      rscid => 18,
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
END v25;

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

ARCHITECTURE v25 OF getMaxLine_core_x1_rsci IS
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
      rscid => 17,
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_heightIn_rsc_triosy_obj IS
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_widthIn_rsc_triosy_obj IS
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_acc_rsci IS
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
END v25;

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

ARCHITECTURE v25 OF houghTransform_core_data_in_rsci IS
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

END v25;

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
    cos_out_1_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    cos_out_1_rsci_addr_d : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    cos_out_1_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    cos_out_1_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    cos_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    cos_out_1_rsci_en_d : OUT STD_LOGIC;
    sin_out_1_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    sin_out_1_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    sin_out_1_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    sin_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_a : OUT STD_LOGIC_VECTOR (17 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_b : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0)
  );
END getMaxLine_core;

ARCHITECTURE v25 OF getMaxLine_core IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL core_wen : STD_LOGIC;
  SIGNAL x1_rsci_wen_comp : STD_LOGIC;
  SIGNAL y1_rsci_wen_comp : STD_LOGIC;
  SIGNAL x2_rsci_wen_comp : STD_LOGIC;
  SIGNAL y2_rsci_wen_comp : STD_LOGIC;
  SIGNAL acc_rsci_wen_comp : STD_LOGIC;
  SIGNAL acc_rsci_idat_mxwt : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_1_rsci_data_out_d_oreg : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL sin_out_1_rsci_data_out_d_oreg : STD_LOGIC_VECTOR (26 DOWNTO 0);
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
  SIGNAL fsm_output : STD_LOGIC_VECTOR (21 DOWNTO 0);
  SIGNAL or_dcpl_8 : STD_LOGIC;
  SIGNAL or_dcpl_11 : STD_LOGIC;
  SIGNAL or_dcpl_23 : STD_LOGIC;
  SIGNAL and_dcpl_11 : STD_LOGIC;
  SIGNAL and_dcpl_21 : STD_LOGIC;
  SIGNAL or_dcpl_34 : STD_LOGIC;
  SIGNAL and_dcpl_71 : STD_LOGIC;
  SIGNAL or_dcpl_63 : STD_LOGIC;
  SIGNAL or_dcpl_64 : STD_LOGIC;
  SIGNAL and_dcpl_75 : STD_LOGIC;
  SIGNAL or_dcpl_73 : STD_LOGIC;
  SIGNAL or_dcpl_77 : STD_LOGIC;
  SIGNAL and_dcpl_79 : STD_LOGIC;
  SIGNAL or_dcpl_83 : STD_LOGIC;
  SIGNAL or_dcpl_84 : STD_LOGIC;
  SIGNAL or_dcpl_91 : STD_LOGIC;
  SIGNAL or_dcpl_92 : STD_LOGIC;
  SIGNAL or_dcpl_94 : STD_LOGIC;
  SIGNAL or_dcpl_104 : STD_LOGIC;
  SIGNAL or_dcpl_114 : STD_LOGIC;
  SIGNAL or_dcpl_115 : STD_LOGIC;
  SIGNAL or_tmp_139 : STD_LOGIC;
  SIGNAL or_tmp_140 : STD_LOGIC;
  SIGNAL or_tmp_141 : STD_LOGIC;
  SIGNAL or_tmp_142 : STD_LOGIC;
  SIGNAL or_tmp_163 : STD_LOGIC;
  SIGNAL or_tmp_164 : STD_LOGIC;
  SIGNAL or_tmp_211 : STD_LOGIC;
  SIGNAL or_tmp_217 : STD_LOGIC;
  SIGNAL and_290_cse : STD_LOGIC;
  SIGNAL and_382_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      : STD_LOGIC;
  SIGNAL T_LINE_slc_T_LINE_acc_6_itm : STD_LOGIC;
  SIGNAL T_LINE_if_land_lpi_3_dfm : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1
      : STD_LOGIC;
  SIGNAL operator_27_3_true_AC_TRN_AC_WRAP_return_sva : STD_LOGIC;
  SIGNAL operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL for_asn_7_itm_3 : STD_LOGIC;
  SIGNAL for_stage_0_4 : STD_LOGIC;
  SIGNAL for_stage_0_2 : STD_LOGIC;
  SIGNAL for_stage_0 : STD_LOGIC;
  SIGNAL for_stage_0_3 : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_3_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_1_itm
      : STD_LOGIC;
  SIGNAL for_stage_0_5 : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_2_itm
      : STD_LOGIC;
  SIGNAL for_asn_7_itm_2 : STD_LOGIC;
  SIGNAL for_asn_7_itm_1 : STD_LOGIC;
  SIGNAL exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_5_itm
      : STD_LOGIC;
  SIGNAL for_stage_0_6 : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_4_itm
      : STD_LOGIC;
  SIGNAL for_asn_7_itm_4 : STD_LOGIC;
  SIGNAL T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva : STD_LOGIC_VECTOR
      (26 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_else_dividend1_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend2_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL T_LINE_if_else_dividend2_sva : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      : STD_LOGIC;
  SIGNAL and_224_ssc : STD_LOGIC;
  SIGNAL and_226_ssc : STD_LOGIC;
  SIGNAL asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc
      : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 : STD_LOGIC;
  SIGNAL reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 : STD_LOGIC;
  SIGNAL Hough_Algorithm_HW_1296_864_getMaxLine_and_cse : STD_LOGIC;
  SIGNAL reg_sin_out_1_rsc_cgo_cse : STD_LOGIC;
  SIGNAL reg_acc_rsci_irdy_core_psct_cse : STD_LOGIC;
  SIGNAL reg_y2_rsci_ivld_core_psct_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse
      : STD_LOGIC;
  SIGNAL T_LINE_and_4 : STD_LOGIC;
  SIGNAL T_LINE_T_LINE_nor_3 : STD_LOGIC;
  SIGNAL x1_t_26_18_lpi_3 : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL R_LINE_r_10_0_sva : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
      : STD_LOGIC;
  SIGNAL reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse
      : STD_LOGIC;
  SIGNAL or_252_rmff : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_000000
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_sva_3_1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_sva_3_1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0
      : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL T_LINE_t_7_0_sva : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_itm :
      STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL or_tmp_399 : STD_LOGIC;
  SIGNAL z_out_2 : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL z_out_3 : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL or_tmp_410 : STD_LOGIC;
  SIGNAL z_out_4 : STD_LOGIC_VECTOR (43 DOWNTO 0);
  SIGNAL z_out_5 : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL z_out_6 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL z_out_7 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL z_out_8 : STD_LOGIC_VECTOR (23 DOWNTO 0);
  SIGNAL z_out_9 : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL z_out_10 : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL for_t_7_0_sva : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_3
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL threshold_23_8_lpi_3 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL x1_t_13_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_14_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_12_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_15_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_11_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_16_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_10_lpi_3 : STD_LOGIC;
  SIGNAL x1_t_17_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_14_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_13_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_15_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_12_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_16_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_11_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_17_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_10_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_18_lpi_3 : STD_LOGIC;
  SIGNAL y1_t_25_19_lpi_3 : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL x2_t_13_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_14_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_12_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_15_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_11_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_16_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_10_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_17_lpi_3 : STD_LOGIC;
  SIGNAL x2_t_26_18_lpi_3 : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL y2_t_14_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_13_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_15_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_12_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_16_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_11_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_17_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_10_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_18_lpi_3 : STD_LOGIC;
  SIGNAL y2_t_25_19_lpi_3 : STD_LOGIC_VECTOR (6 DOWNTO 0);
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
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
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
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
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
  SIGNAL for_t_7_0_sva_1_1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_2
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_3
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_return_1_69_38_sva_1 : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_return_1_69_38_sva_2 : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_return_1_69_38_sva_3 : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL for_asn_5_itm_1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_42_17_25_24_itm_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_6_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_7_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_8_itm
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_dfm_1_4_0
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL for_acc_2_itm_1_20_7 : STD_LOGIC_VECTOR (13 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_mx1
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0
      : STD_LOGIC;
  SIGNAL for_t_7_0_sva_mx1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_25_5_mx0
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0_mx0
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL for_acc_5_psp_1 : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2
      : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2
      : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_34_32_1
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (27 DOWNTO 0);
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
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0
      : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_26_25
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_23_22
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_20_18
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_9_8
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_6_5
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_2_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_32_29
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_26
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_25
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_23
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_22
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_20
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_19
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_18
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_9
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_8
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_6
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_5
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_6_itm_25_0
      : STD_LOGIC_VECTOR (25 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_8_itm_25_0
      : STD_LOGIC_VECTOR (25 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_1_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_rgt
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_ssc
      : STD_LOGIC;
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      : STD_LOGIC;
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2
      : STD_LOGIC_VECTOR (23 DOWNTO 0);
  SIGNAL and_971_ssc : STD_LOGIC;
  SIGNAL and_973_ssc : STD_LOGIC;
  SIGNAL and_975_ssc : STD_LOGIC;
  SIGNAL and_977_ssc : STD_LOGIC;
  SIGNAL for_and_1_cse : STD_LOGIC;
  SIGNAL for_and_5_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_1_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_and_1_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_and_cse
      : STD_LOGIC;
  SIGNAL or_523_cse : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_2_cse
      : STD_LOGIC;
  SIGNAL x1_t_and_2_cse : STD_LOGIC;
  SIGNAL x2_t_and_3_cse : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_and_5_rgt
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_rgt
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_26_21
      : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_20_0
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_1_reg
      : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL or_574_cse : STD_LOGIC;
  SIGNAL and_1012_cse : STD_LOGIC;
  SIGNAL or_564_cse : STD_LOGIC;
  SIGNAL mux_19_cse : STD_LOGIC;
  SIGNAL y2_t_or_itm : STD_LOGIC;
  SIGNAL operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      : STD_LOGIC;
  SIGNAL for_acc_itm_6_1 : STD_LOGIC;
  SIGNAL nand_12_cse : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_or_1_cse : STD_LOGIC;

  SIGNAL acc_nl : STD_LOGIC_VECTOR (33 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_2_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_48_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL acc_1_nl : STD_LOGIC_VECTOR (33 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nand_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_49_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL mux_nl : STD_LOGIC;
  SIGNAL nor_55_nl : STD_LOGIC;
  SIGNAL nand_9_nl : STD_LOGIC;
  SIGNAL for_acc_7_nl : STD_LOGIC_VECTOR (13 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL or_266_nl : STD_LOGIC;
  SIGNAL and_206_nl : STD_LOGIC;
  SIGNAL or_270_nl : STD_LOGIC;
  SIGNAL or_272_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_2_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_4_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_23_nl
      : STD_LOGIC;
  SIGNAL T_LINE_t_mux1h_3_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL nor_38_nl : STD_LOGIC;
  SIGNAL nor_57_nl : STD_LOGIC;
  SIGNAL T_LINE_if_aelse_T_LINE_if_aelse_and_6_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_4_nl
      : STD_LOGIC;
  SIGNAL T_LINE_if_T_LINE_if_and_17_nl : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL mux_18_nl : STD_LOGIC;
  SIGNAL y2_t_and_1_nl : STD_LOGIC;
  SIGNAL operator_28_13_true_AC_TRN_AC_WRAP_acc_2_nl : STD_LOGIC_VECTOR (8 DOWNTO
      0);
  SIGNAL T_LINE_if_T_LINE_if_and_nl : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL T_LINE_if_aelse_not_39_nl : STD_LOGIC;
  SIGNAL and_563_nl : STD_LOGIC;
  SIGNAL y1_t_and_1_nl : STD_LOGIC;
  SIGNAL x2_t_and_1_nl : STD_LOGIC;
  SIGNAL R_LINE_r_R_LINE_r_and_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_1_nl
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL not_386_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_nl
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_and_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_24_nl
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
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_66_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_26_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_28_nl
      : STD_LOGIC;
  SIGNAL operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl : STD_LOGIC_VECTOR
      (16 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_nl
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL or_128_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl
      : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_50_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_88_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_47_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_95_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_8_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_76_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_43_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_6_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_78_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_39_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_4_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_79_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_35_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_32_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_56_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_54_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_36_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_53_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_40_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_77_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_44_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl
      : STD_LOGIC_VECTOR (27 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_87_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_55_nl
      : STD_LOGIC;
  SIGNAL for_acc_nl : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_or_nl : STD_LOGIC;
  SIGNAL T_LINE_if_if_dividend1_or_1_nl : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_7_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_44_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_45_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_8_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_46_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_47_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_9_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_48_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_49_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_10_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_50_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_51_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_11_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_52_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_53_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_12_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_54_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_55_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_mux_13_nl : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_56_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_57_nl
      : STD_LOGIC;
  SIGNAL acc_3_nl : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_6_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_6_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL not_424_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_7_nl
      : STD_LOGIC_VECTOR (23 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_7_nl
      : STD_LOGIC_VECTOR (23 DOWNTO 0);
  SIGNAL not_425_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_8_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_9_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_10_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_11_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_2_nl
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_3_nl
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_2_nl
      : STD_LOGIC_VECTOR (43 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_4_nl
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      : STD_LOGIC_VECTOR (39 DOWNTO 0);
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_1_nl : STD_LOGIC_VECTOR (8 DOWNTO
      0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_29_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_58_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_59_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_60_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_61_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_62_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_30_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_63_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_64_nl
      : STD_LOGIC;
  SIGNAL operator_20_15_true_AC_TRN_AC_WRAP_2_mux_1_nl : STD_LOGIC_VECTOR (6 DOWNTO
      0);
  SIGNAL T_LINE_mux1h_4_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL T_LINE_T_LINE_mux_2_nl : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL T_LINE_or_3_nl : STD_LOGIC;
  SIGNAL T_LINE_mux_2_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL for_for_and_2_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL for_not_10_nl : STD_LOGIC;
  SIGNAL for_for_and_3_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_not_11_nl : STD_LOGIC;
  SIGNAL for_mux_7_nl : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL for_mux_8_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_for_or_1_nl : STD_LOGIC_VECTOR (13 DOWNTO 0);
  SIGNAL for_acc_8_nl : STD_LOGIC_VECTOR (13 DOWNTO 0);
  SIGNAL for_mux_9_nl : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_nor_1_nl
      : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_2_nl
      : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_or_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_3_nl
      : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_13_nl
      : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_14_nl
      : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
      : STD_LOGIC_VECTOR (31 DOWNTO 0);

  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a : STD_LOGIC_VECTOR (32 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s : STD_LOGIC_VECTOR (4 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_z : STD_LOGIC_VECTOR (32 DOWNTO
      0);

  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a : STD_LOGIC_VECTOR (32 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s : STD_LOGIC_VECTOR (4 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z : STD_LOGIC_VECTOR (32 DOWNTO
      0);

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
      cos_out_1_rsc_cgo_iro : IN STD_LOGIC;
      cos_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      cos_out_1_rsci_en_d : OUT STD_LOGIC;
      sin_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0);
      core_wen : IN STD_LOGIC;
      cos_out_1_rsc_cgo : IN STD_LOGIC;
      cos_out_1_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      sin_out_1_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_z_oreg : OUT STD_LOGIC_VECTOR (43 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_wait_dp_inst_cos_out_1_rsci_data_out_d : STD_LOGIC_VECTOR
      (53 DOWNTO 0);
  SIGNAL getMaxLine_core_wait_dp_inst_sin_out_1_rsci_data_out_d : STD_LOGIC_VECTOR
      (53 DOWNTO 0);
  SIGNAL getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z : STD_LOGIC_VECTOR
      (43 DOWNTO 0);
  SIGNAL getMaxLine_core_wait_dp_inst_cos_out_1_rsci_data_out_d_oreg : STD_LOGIC_VECTOR
      (26 DOWNTO 0);
  SIGNAL getMaxLine_core_wait_dp_inst_sin_out_1_rsci_data_out_d_oreg : STD_LOGIC_VECTOR
      (26 DOWNTO 0);
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
      fsm_output : OUT STD_LOGIC_VECTOR (21 DOWNTO 0);
      for_C_0_tr0 : IN STD_LOGIC;
      T_LINE_C_5_tr0 : IN STD_LOGIC;
      T_LINE_C_5_tr1 : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
          : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
          : IN STD_LOGIC;
      T_LINE_C_9_tr0 : IN STD_LOGIC;
      T_LINE_C_9_tr1 : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
          : IN STD_LOGIC;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
          : IN STD_LOGIC;
      T_LINE_C_13_tr0 : IN STD_LOGIC;
      R_LINE_C_0_tr0 : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_core_fsm_inst_fsm_output : STD_LOGIC_VECTOR (21 DOWNTO 0);
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_5_tr0 : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_9_tr0 : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      : STD_LOGIC;
  SIGNAL getMaxLine_core_core_fsm_inst_T_LINE_C_13_tr0 : STD_LOGIC;
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

  FUNCTION MUX1HOT_v_19_8_2(input_7 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_6 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_5 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_4 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(7 DOWNTO 0))
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
      tmp := (OTHERS=>sel( 6));
      result := result or ( input_6 and tmp);
      tmp := (OTHERS=>sel( 7));
      result := result or ( input_7 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_24_3_2(input_2 : STD_LOGIC_VECTOR(23 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(23 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(23 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(23 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(23 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
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

  FUNCTION MUX1HOT_v_27_4_2(input_3 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(26 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(26 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(26 DOWNTO 0);

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

  FUNCTION MUX1HOT_v_32_3_2(input_2 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(31 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
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

  FUNCTION MUX1HOT_v_4_3_2(input_2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
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
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_7_4_2(input_3 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(6 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(6 DOWNTO 0);

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

  FUNCTION MUX1HOT_v_8_3_2(input_2 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(7 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(7 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_9_3_2(input_2 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(8 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(8 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_9_4_2(input_3 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(8 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(8 DOWNTO 0);

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

  FUNCTION MUX1HOT_v_9_5_2(input_4 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(4 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(8 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(8 DOWNTO 0);

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

  FUNCTION MUX_v_14_2_2(input_0 : STD_LOGIC_VECTOR(13 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(13 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(13 DOWNTO 0);

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

  FUNCTION MUX_v_21_2_2(input_0 : STD_LOGIC_VECTOR(20 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(20 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(20 DOWNTO 0);

    BEGIN
      CASE sel IS
        WHEN '0' =>
          result := input_0;
        WHEN others =>
          result := input_1;
      END CASE;
    RETURN result;
  END;

  FUNCTION MUX_v_24_2_2(input_0 : STD_LOGIC_VECTOR(23 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(23 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(23 DOWNTO 0);

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

  FUNCTION MUX_v_9_2_2(input_0 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(8 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(8 DOWNTO 0);

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
  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg : work.hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60_pkg.hough_algorithm_hw_1296_864mgc_rom_27_70_32_1_60
    PORT MAP(
      addr => ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr,
      data_out => ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
    );
  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr <= STD_LOGIC_VECTOR(UNSIGNED'(
      "00") & UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1_1));
  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_itm <= ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out;

  operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg : work.mgc_shift_comps_v5.mgc_shift_r_v5
    GENERIC MAP(
      width_a => 33,
      signd_a => 1,
      width_s => 5,
      width_z => 33
      )
    PORT MAP(
      a => operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a,
      s => operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s,
      z => operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_z
    );
  operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_32_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_26
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_23
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_20
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_19
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_9
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_6
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_2
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_0;
  operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1
      <= operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_z;

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
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_32_30
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_26_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_23_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_20_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_9_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_6_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_2_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_0;
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1
      <= operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z;

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
      cos_out_1_rsc_cgo_iro => or_252_rmff,
      cos_out_1_rsci_data_out_d => getMaxLine_core_wait_dp_inst_cos_out_1_rsci_data_out_d,
      cos_out_1_rsci_en_d => cos_out_1_rsci_en_d,
      sin_out_1_rsci_data_out_d => getMaxLine_core_wait_dp_inst_sin_out_1_rsci_data_out_d,
      T_LINE_if_if_dividend1_mul_cmp_z => getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z,
      core_wen => core_wen,
      cos_out_1_rsc_cgo => reg_sin_out_1_rsc_cgo_cse,
      cos_out_1_rsci_data_out_d_oreg => getMaxLine_core_wait_dp_inst_cos_out_1_rsci_data_out_d_oreg,
      sin_out_1_rsci_data_out_d_oreg => getMaxLine_core_wait_dp_inst_sin_out_1_rsci_data_out_d_oreg,
      T_LINE_if_if_dividend1_mul_cmp_z_oreg => getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z_oreg
    );
  getMaxLine_core_wait_dp_inst_cos_out_1_rsci_data_out_d <= cos_out_1_rsci_data_out_d;
  getMaxLine_core_wait_dp_inst_sin_out_1_rsci_data_out_d <= sin_out_1_rsci_data_out_d;
  getMaxLine_core_wait_dp_inst_T_LINE_if_if_dividend1_mul_cmp_z <= T_LINE_if_if_dividend1_mul_cmp_z;
  cos_out_1_rsci_data_out_d_oreg <= getMaxLine_core_wait_dp_inst_cos_out_1_rsci_data_out_d_oreg;
  sin_out_1_rsci_data_out_d_oreg <= getMaxLine_core_wait_dp_inst_sin_out_1_rsci_data_out_d_oreg;
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
      for_C_0_tr0 => and_dcpl_71,
      T_LINE_C_5_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_5_tr0,
      T_LINE_C_5_tr1 => T_LINE_and_4,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_9_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_9_tr0,
      T_LINE_C_9_tr1 => T_LINE_and_4,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
          => getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_13_tr0 => getMaxLine_core_core_fsm_inst_T_LINE_C_13_tr0,
      R_LINE_C_0_tr0 => getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0
    );
  fsm_output <= getMaxLine_core_core_fsm_inst_fsm_output;
  getMaxLine_core_core_fsm_inst_T_LINE_C_5_tr0 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1;
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      <= NOT (z_out_3(4));
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      <= NOT (z_out_3(4));
  getMaxLine_core_core_fsm_inst_T_LINE_C_9_tr0 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1;
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      <= NOT (z_out_3(4));
  getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      <= NOT (z_out_3(4));
  getMaxLine_core_core_fsm_inst_T_LINE_C_13_tr0 <= NOT T_LINE_slc_T_LINE_acc_6_itm;
  getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0 <= NOT (z_out_10(8));

  for_and_1_cse <= core_wen AND for_stage_0_3;
  or_564_cse <= (NOT for_stage_0_4) OR for_asn_7_itm_3 OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_cse
      <= core_wen AND for_stage_0_2;
  for_and_5_cse <= core_wen AND for_stage_0;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      <= core_wen AND (fsm_output(8));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      <= core_wen AND (fsm_output(9));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      <= core_wen AND (NOT or_dcpl_8) AND (fsm_output(12));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      <= core_wen AND (NOT or_dcpl_11) AND (fsm_output(12));
  Hough_Algorithm_HW_1296_864_getMaxLine_and_cse <= core_wen AND (NOT((NOT (fsm_output(20)))
      OR (z_out_10(8))));
  or_252_rmff <= and_290_cse OR ((and_dcpl_75 OR ((NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_5_itm)
      AND for_stage_0_6)) AND (fsm_output(1))) OR and_382_cse;
  T_LINE_if_if_dividend1_mul_cmp_a <= reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd &
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8
      & reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_1_cse
      <= core_wen AND (and_dcpl_11 OR (NOT for_asn_7_itm_3) OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_3_itm))
      AND for_stage_0_4;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_cse <=
      core_wen AND (fsm_output(1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_2_cse
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_cse
      AND (NOT((NOT(or_dcpl_23 AND for_asn_7_itm_3)) AND for_stage_0_4)) AND (NOT(or_dcpl_23
      AND for_asn_7_itm_4)) AND for_stage_0_5;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_and_1_cse
      <= core_wen AND (and_dcpl_21 OR (NOT(for_asn_7_itm_2 AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_2_itm)))
      AND for_stage_0_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse <=
      (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1)
      AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_1_rgt
      <= (T_LINE_if_land_lpi_3_dfm AND or_574_cse) OR ((NOT T_LINE_if_land_lpi_3_dfm)
      AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c);
  x1_t_and_2_cse <= core_wen AND or_tmp_164;
  x2_t_and_3_cse <= core_wen AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
      AND (fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c
      <= (fsm_output(9)) OR (fsm_output(15));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_rgt
      <= (NOT((NOT (z_out_3(4))) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      OR T_LINE_if_land_lpi_3_dfm)) AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_rgt
      <= (NOT or_dcpl_11) AND (fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      <= (NOT T_LINE_if_land_lpi_3_dfm) AND (fsm_output(7));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      <= T_LINE_if_land_lpi_3_dfm AND (fsm_output(7));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      <= (NOT T_LINE_if_land_lpi_3_dfm) AND (fsm_output(13));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      <= T_LINE_if_land_lpi_3_dfm AND (fsm_output(13));
  y2_t_or_itm <= T_LINE_and_4 OR T_LINE_T_LINE_nor_3;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_1_nl
      <= MUX_v_27_2_2((STD_LOGIC_VECTOR'( "000000") & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_25_5_mx0),
      (z_out_4(26 DOWNTO 0)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_1_rgt);
  not_386_nl <= NOT or_tmp_211;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_and_5_rgt
      <= MUX_v_27_2_2(STD_LOGIC_VECTOR'("000000000000000000000000000"), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_1_nl,
      not_386_nl);
  or_574_cse <= (fsm_output(14)) OR (fsm_output(8));
  mux_19_cse <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c,
      or_574_cse, T_LINE_if_land_lpi_3_dfm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_nl <=
      MUX_v_21_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_25_5_mx0,
      for_acc_5_psp_1, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_rgt
      <= MUX_v_27_2_2((STD_LOGIC_VECTOR'( "000000") & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_nl),
      (z_out_4(26 DOWNTO 0)), or_dcpl_94);
  and_1012_cse <= (NOT(CONV_SL_1_1(fsm_output(8 DOWNTO 7)/=STD_LOGIC_VECTOR'("00"))))
      AND (NOT(CONV_SL_1_1(fsm_output(15 DOWNTO 14)/=STD_LOGIC_VECTOR'("00")))) AND
      (NOT((fsm_output(12)) OR (fsm_output(6)))) AND (NOT((fsm_output(9)) OR (fsm_output(13))));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_nl
      <= ((NOT T_LINE_if_land_lpi_3_dfm) AND (cos_out_1_rsci_data_out_d_oreg(26)))
      OR (T_LINE_if_land_lpi_3_dfm AND (sin_out_1_rsci_data_out_d_oreg(26)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_6_itm_25_0
      <= MUX1HOT_v_26_3_2((z_out_9(25 DOWNTO 0)), (cos_out_1_rsci_data_out_d_oreg(25
      DOWNTO 0)), (sin_out_1_rsci_data_out_d_oreg(25 DOWNTO 0)), STD_LOGIC_VECTOR'(
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_nl
      & asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      & and_224_ssc));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      <= (T_LINE_if_land_lpi_3_dfm AND (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(26)))
      OR ((NOT T_LINE_if_land_lpi_3_dfm) AND (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(26)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_8_itm_25_0
      <= MUX1HOT_v_26_3_2((z_out_9(25 DOWNTO 0)), (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(25
      DOWNTO 0)), (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(25 DOWNTO
      0)), STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      & and_226_ssc & asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_ssc
      <= core_wen AND (NOT(or_dcpl_104 OR or_dcpl_92 OR (fsm_output(7)) OR or_dcpl_91));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      <= (or_dcpl_114 AND or_574_cse) OR (or_dcpl_115 AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c)
      OR or_tmp_163;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      <= (NOT or_dcpl_114) AND or_574_cse;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse
      <= (NOT or_dcpl_115) AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_and_cse
      <= core_wen AND T_LINE_T_LINE_nor_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_mx1
      <= MUX_v_35_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_3,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2,
      or_dcpl_63);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_42_17_25_24_itm_1(0))
      XOR (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_42_17_25_24_itm_1(1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs,
      or_dcpl_64);
  operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED('1'
      & acc_rsci_idat_mxwt) + CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(NOT threshold_23_8_lpi_3),
      16), 17) + UNSIGNED'( "00000000000000001"), 17));
  operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1 <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl(16);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_nl <=
      STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED((NOT for_acc_5_psp_1)
      & (NOT (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2(4
      DOWNTO 0)))), 26), 27) + UNSIGNED'( "001000000000000000000000001"), 27));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_nl(26);
  or_128_nl <= (NOT for_stage_0_2) OR for_asn_7_itm_1 OR T_LINE_slc_T_LINE_acc_6_itm;
  for_t_7_0_sva_mx1 <= MUX_v_8_2_2(for_t_7_0_sva_1_1, for_t_7_0_sva, or_128_nl);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1
      <= NOT(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
      AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_25_5_mx0
      <= MUX_v_21_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_20_0,
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg,
      or_dcpl_73);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0_mx0
      <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_dfm_1_4_0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0,
      or_dcpl_73);
  for_acc_5_psp_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2(23
      DOWNTO 5)), 19), 21) + UNSIGNED((for_asn_5_itm_1(6 DOWNTO 0)) & STD_LOGIC_VECTOR'(
      "000000") & for_asn_5_itm_1), 21));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1
      <= MUX_v_33_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_sva_3_1,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2,
      or_dcpl_63);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1
      <= MUX_v_33_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_sva_3_1,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2,
      or_dcpl_63);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_nl <=
      NOT exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1_1
      <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl <=
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & (z_out_10(4 DOWNTO 2))) + SIGNED'(
      "0001"), 4));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl(3);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl
      <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED((NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_34_32_1)
      & (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1)),
      35), 36) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_20_0
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_dfm_1_4_0
      & STD_LOGIC_VECTOR'( "00000000")), 34), 36) + UNSIGNED'( "000000000000000000000000000000000001"),
      36));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl(35);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2
      <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(CONV_UNSIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm,
      1), 1), 17) + SIGNED'( (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1)),
      17));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2
      <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(CONV_UNSIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm,
      1), 1), 16) + SIGNED'( (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1)
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
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1)
      & (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1)),
      16));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_34_32_1
      <= MUX1HOT_v_3_3_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_mx1(34
      DOWNTO 32)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),3)), STD_LOGIC_VECTOR'( "001"), STD_LOGIC_VECTOR'( (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3)
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_mx1(31
      DOWNTO 0)) AND STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp,
      1),32)) AND STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1,
      1),32));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_3_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1
      <= NOT(MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1,
      1),3)), STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_3_nl));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      AND reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1
      <= NOT(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse
      OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_50_nl
      <= MUX_v_3_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(32
      DOWNTO 30)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),3)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_88_nl
      <= NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_32_30
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_50_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_88_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_29
      <= ((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(29))
      AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm))
      OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_47_nl
      <= MUX_v_2_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(28
      DOWNTO 27)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),2)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_95_nl
      <= NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_28_27
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_47_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_95_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_76_nl
      <= NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_8_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(26
      DOWNTO 25)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_76_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_26_25
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_8_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_43_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(24)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_24
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_43_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_78_nl
      <= NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_6_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(23
      DOWNTO 22)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_78_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_23_22
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_6_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_39_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(21)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_21
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_39_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_79_nl
      <= NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_4_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(20
      DOWNTO 18)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_79_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_20_18
      <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_4_nl,
      STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_35_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(17)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_17
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_35_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_16
      <= ((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(16))
      AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm))
      OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_32_nl
      <= MUX_v_2_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(15
      DOWNTO 14)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),2)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_56_nl
      <= NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_15_14
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_32_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_56_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_54_nl
      <= NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(13
      DOWNTO 11)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_54_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_13_11
      <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_nl,
      STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_36_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(10)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_10
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_36_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_53_nl
      <= NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(9
      DOWNTO 8)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_53_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_9_8
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_40_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(7)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_7
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_40_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_77_nl
      <= NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(6
      DOWNTO 5)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_77_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_6_5
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_44_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(4)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_4
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_44_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_3
      <= ((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(3))
      AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm))
      OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_2_1
      <= MUX1HOT_v_2_3_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(2
      DOWNTO 1)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),2)), STD_LOGIC_VECTOR'( "01"), STD_LOGIC_VECTOR'( (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse)
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_0
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_mx1(0))
      AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm)
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_if_dividend1_sva(18 DOWNTO 0))), (z_out_9(19
      DOWNTO 0)), T_LINE_if_if_dividend1_sva(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_else_dividend1_sva(18 DOWNTO 0))), (z_out_9(19
      DOWNTO 0)), T_LINE_if_else_dividend1_sva(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl
      <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_1_reg
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg)
      + CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2),
      27), 28), 28));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0
      <= MUX_v_28_2_2(('0' & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_1_reg
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl),
      28)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
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
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
      OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_if_dividend2_sva(18 DOWNTO 0))), (z_out_9(19
      DOWNTO 0)), T_LINE_if_if_dividend2_sva(19));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      <= MUX_v_20_2_2(('0' & (T_LINE_if_else_dividend2_sva(18 DOWNTO 0))), (z_out_9(19
      DOWNTO 0)), T_LINE_if_else_dividend2_sva(19));
  T_LINE_and_4 <= T_LINE_if_land_lpi_3_dfm AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1);
  T_LINE_T_LINE_nor_3 <= NOT(T_LINE_if_land_lpi_3_dfm OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_32_29
      <= MUX_v_4_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(32
      DOWNTO 29)), (STD_LOGIC_VECTOR'( "000") & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_87_nl
      <= NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_28_27
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(28
      DOWNTO 27)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_87_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_26
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(26)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_25
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(25)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_24
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(24))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_23
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(23)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_22
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(22)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_21
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(21))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_20
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(20)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_19
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(19)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_18
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(18)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_17
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(17))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_16
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(16)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1,
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_55_nl
      <= NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_15_14
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(15
      DOWNTO 14)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_55_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_13_11
      <= MUX_v_3_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(13
      DOWNTO 11)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1,
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_10
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(10))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_9
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(9)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_8
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(8)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_7
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(7))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_6
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(6)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_5
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(5)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_1(2)),
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_4
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(4))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_3
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(3)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1,
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_2
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(2))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_1
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(1)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1,
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_0
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_mx1(0))
      AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse);
  or_dcpl_8 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      OR (NOT T_LINE_if_land_lpi_3_dfm);
  or_dcpl_11 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      OR T_LINE_if_land_lpi_3_dfm;
  or_dcpl_23 <= for_stage_0_3 OR for_stage_0 OR for_stage_0_2;
  and_dcpl_11 <= NOT(for_stage_0_3 OR for_stage_0 OR for_stage_0_2);
  and_dcpl_21 <= NOT(for_stage_0 OR for_stage_0_2);
  or_dcpl_34 <= NOT(for_stage_0 AND for_asn_7_itm_1);
  and_dcpl_71 <= NOT(for_stage_0 OR for_stage_0_3 OR for_stage_0_2 OR for_stage_0_5
      OR for_stage_0_4);
  or_dcpl_63 <= (NOT for_stage_0_5) OR for_asn_7_itm_4;
  or_dcpl_64 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
      OR (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3);
  and_dcpl_75 <= for_stage_0_5 AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_4_itm);
  or_dcpl_73 <= (NOT for_stage_0_4) OR for_asn_7_itm_3;
  or_dcpl_77 <= CONV_SL_1_1(fsm_output(15 DOWNTO 13)/=STD_LOGIC_VECTOR'("000"));
  and_dcpl_79 <= NOT((fsm_output(0)) OR (fsm_output(21)));
  or_dcpl_83 <= (fsm_output(10)) OR (fsm_output(16));
  or_dcpl_84 <= (fsm_output(17)) OR (fsm_output(11));
  or_dcpl_91 <= CONV_SL_1_1(fsm_output(9 DOWNTO 8)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_92 <= CONV_SL_1_1(fsm_output(15 DOWNTO 14)/=STD_LOGIC_VECTOR'("00"));
  or_dcpl_94 <= (fsm_output(5)) OR (fsm_output(11));
  or_dcpl_104 <= or_dcpl_94 OR (fsm_output(6)) OR (fsm_output(12)) OR (fsm_output(13));
  or_dcpl_114 <= NOT((z_out_3(4)) AND T_LINE_if_land_lpi_3_dfm);
  or_dcpl_115 <= (NOT (z_out_3(4))) OR T_LINE_if_land_lpi_3_dfm;
  and_290_cse <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
      AND (fsm_output(3));
  and_382_cse <= (NOT operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1)
      AND (fsm_output(2));
  or_tmp_139 <= T_LINE_if_land_lpi_3_dfm AND (fsm_output(4));
  or_tmp_140 <= (NOT T_LINE_if_land_lpi_3_dfm) AND (fsm_output(4));
  or_tmp_141 <= T_LINE_if_land_lpi_3_dfm AND (fsm_output(10));
  or_tmp_142 <= (NOT T_LINE_if_land_lpi_3_dfm) AND (fsm_output(10));
  or_tmp_163 <= or_dcpl_84 OR or_dcpl_83;
  or_tmp_164 <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
      AND (fsm_output(16));
  or_tmp_211 <= (fsm_output(13)) OR (fsm_output(7));
  or_tmp_217 <= or_dcpl_92 OR or_dcpl_91;
  for_acc_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(z_out_7(7
      DOWNTO 2)), 6), 7) + SIGNED'( "1010011"), 7));
  for_acc_itm_6_1 <= for_acc_nl(6);
  asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      <= NOT(T_LINE_if_land_lpi_3_dfm OR (cos_out_1_rsci_data_out_d_oreg(26)));
  and_224_ssc <= T_LINE_if_land_lpi_3_dfm AND (NOT (sin_out_1_rsci_data_out_d_oreg(26)));
  and_226_ssc <= T_LINE_if_land_lpi_3_dfm AND (NOT (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(26)));
  asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc
      <= NOT(T_LINE_if_land_lpi_3_dfm OR (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(26)));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_000000
      <= NOT(and_dcpl_75 AND (fsm_output(1)));
  cos_out_1_rsci_re_d <= STD_LOGIC_VECTOR'( '1' & (NOT and_382_cse));
  cos_out_1_rsci_we_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_000000);
  sin_out_1_rsci_re_d <= STD_LOGIC_VECTOR'( '1' & (NOT and_382_cse));
  sin_out_1_rsci_we_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_000000);
  cos_out_1_rsci_data_in_d <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_sva_3_1(32
      DOWNTO 6);
  cos_out_1_rsci_addr_d <= MUX_v_8_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0,
      T_LINE_t_7_0_sva, fsm_output(2));
  sin_out_1_rsci_data_in_d <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_sva_3_1(32
      DOWNTO 6);
  or_tmp_399 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      AND (fsm_output(1));
  or_523_cse <= (fsm_output(15)) OR (fsm_output(14)) OR (fsm_output(9)) OR (fsm_output(8));
  or_tmp_410 <= (fsm_output(12)) OR (fsm_output(6));
  and_971_ssc <= (NOT(T_LINE_if_land_lpi_3_dfm AND (sin_out_1_rsci_data_out_d_oreg(26))))
      AND (fsm_output(4));
  and_973_ssc <= T_LINE_if_land_lpi_3_dfm AND (sin_out_1_rsci_data_out_d_oreg(26))
      AND (fsm_output(4));
  and_975_ssc <= (T_LINE_if_land_lpi_3_dfm OR (NOT (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(26))))
      AND (fsm_output(10));
  and_977_ssc <= (NOT T_LINE_if_land_lpi_3_dfm) AND (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(26))
      AND (fsm_output(10));
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_63 AND (fsm_output(1))))) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_3;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_asn_7_itm_4 <= '0';
      ELSIF ( (core_wen AND for_stage_0_4) = '1' ) THEN
        for_asn_7_itm_4 <= for_asn_7_itm_3;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
            <= '0';
      ELSIF ( (core_wen AND (NOT((NOT (fsm_output(1))) OR or_dcpl_64 OR (NOT for_stage_0_4))))
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_42_17_25_24_itm_1
            <= STD_LOGIC_VECTOR'( "00");
      ELSIF ( (core_wen AND for_stage_0_3 AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1)
          AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2)
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_target_a_1_42_17_25_24_itm_1
            <= for_acc_5_psp_1(20 DOWNTO 19);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_asn_7_itm_3 <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3
            <= '0';
      ELSIF ( for_and_1_cse = '1' ) THEN
        for_asn_7_itm_3 <= for_asn_7_itm_2;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_dfm_1_4_0
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( (or_564_cse AND for_and_1_cse) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_dfm_1_4_0
            <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0_mx0,
            (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2(4
            DOWNTO 0)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
            <= '0';
      ELSIF ( (core_wen AND (CONV_SL_1_1(fsm_output(2 DOWNTO 1)/=STD_LOGIC_VECTOR'("00"))))
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1,
            operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1, fsm_output(2));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_asn_5_itm_1 <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( (core_wen AND for_stage_0_2 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1)
          = '1' ) THEN
        for_asn_5_itm_1 <= for_t_7_0_sva;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2
            <= '0';
        for_asn_7_itm_2 <= '0';
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1;
        for_asn_7_itm_2 <= for_asn_7_itm_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_t_7_0_sva <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( ((NOT((NOT((NOT(T_LINE_slc_T_LINE_acc_6_itm OR for_asn_7_itm_1)) AND
          for_stage_0_2)) AND (fsm_output(1)))) AND core_wen) = '1' ) THEN
        for_t_7_0_sva <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), for_t_7_0_sva_mx1,
            (fsm_output(1)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_t_7_0_sva_1_1 <= STD_LOGIC_VECTOR'( "00000000");
      ELSIF ( (core_wen AND for_stage_0 AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1)
          AND for_acc_itm_6_1) = '1' ) THEN
        for_t_7_0_sva_1_1 <= z_out_7;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_asn_7_itm_1 <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
            <= '0';
        ac_math_atan_pi_2mi_return_1_69_38_sva_1 <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( for_and_5_cse = '1' ) THEN
        for_asn_7_itm_1 <= NOT(for_acc_itm_6_1 OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1);
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
            <= exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva;
        ac_math_atan_pi_2mi_return_1_69_38_sva_1 <= ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_itm;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_if_dividend1_sva <= STD_LOGIC_VECTOR'( "00000000000000000000");
      ELSIF ( (core_wen AND (NOT or_dcpl_8) AND (fsm_output(6))) = '1' ) THEN
        T_LINE_if_if_dividend1_sva <= z_out_4(43 DOWNTO 24);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_else_dividend1_sva <= STD_LOGIC_VECTOR'( "00000000000000000000");
      ELSIF ( (core_wen AND (NOT or_dcpl_11) AND (fsm_output(6))) = '1' ) THEN
        T_LINE_if_else_dividend1_sva <= z_out_4(43 DOWNTO 24);
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
            <= NOT((T_LINE_if_if_dividend1_sva(19)) XOR (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(26)));
        operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= NOT(CONV_SL_1_1(T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva/=STD_LOGIC_VECTOR'("000000000000000000000000000")));
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
            <= NOT((T_LINE_if_else_dividend1_sva(19)) XOR (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(26)));
        operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= NOT(CONV_SL_1_1(T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva/=STD_LOGIC_VECTOR'("000000000000000000000000000")));
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
        T_LINE_if_if_dividend2_sva <= z_out_4(43 DOWNTO 24);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1;
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
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
        T_LINE_if_else_dividend2_sva <= z_out_4(43 DOWNTO 24);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1;
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
            <= '0';
      ELSIF ( (core_wen AND (fsm_output(14))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
            <= NOT((T_LINE_if_if_dividend2_sva(19)) XOR (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(26)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
            <= '0';
      ELSIF ( (core_wen AND (fsm_output(15))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
            <= NOT((T_LINE_if_else_dividend2_sva(19)) XOR (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(26)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_land_lpi_3_dfm <= '0';
      ELSIF ( (core_wen AND and_290_cse) = '1' ) THEN
        T_LINE_if_land_lpi_3_dfm <= NOT((z_out_10(5)) OR (z_out_8(8)));
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
        y2_rsci_idat_0 <= y2_t_15_lpi_3;
        y2_rsci_idat_1 <= y2_t_16_lpi_3;
        y2_rsci_idat_2 <= y2_t_17_lpi_3;
        y2_rsci_idat_3 <= y2_t_18_lpi_3;
        y2_rsci_idat_9_4 <= y2_t_25_19_lpi_3(5 DOWNTO 0);
        x2_rsci_idat_0 <= x2_t_15_lpi_3;
        x2_rsci_idat_1 <= x2_t_16_lpi_3;
        x2_rsci_idat_2 <= x2_t_17_lpi_3;
        x2_rsci_idat_10_3 <= x2_t_26_18_lpi_3(7 DOWNTO 0);
        y1_rsci_idat_0 <= y1_t_15_lpi_3;
        y1_rsci_idat_1 <= y1_t_16_lpi_3;
        y1_rsci_idat_2 <= y1_t_17_lpi_3;
        y1_rsci_idat_3 <= y1_t_18_lpi_3;
        y1_rsci_idat_9_4 <= y1_t_25_19_lpi_3(5 DOWNTO 0);
        x1_rsci_idat_0 <= x1_t_15_lpi_3;
        x1_rsci_idat_1 <= x1_t_16_lpi_3;
        x1_rsci_idat_2 <= x1_t_17_lpi_3;
        x1_rsci_idat_10_3 <= x1_t_26_18_lpi_3(7 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_stage_0 <= '0';
        exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
            <= '0';
        for_stage_0_2 <= '0';
        for_stage_0_3 <= '0';
        for_stage_0_4 <= '0';
        for_stage_0_5 <= '0';
        for_stage_0_6 <= '0';
        reg_sin_out_1_rsc_cgo_cse <= '0';
        reg_acc_rsci_irdy_core_psct_cse <= '0';
        reg_y2_rsci_ivld_core_psct_cse <= '0';
        T_LINE_if_if_dividend1_mul_cmp_b <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
        for_acc_2_itm_1_20_7 <= STD_LOGIC_VECTOR'( "00000000000000");
        threshold_23_8_lpi_3 <= STD_LOGIC_VECTOR'( "0000000000000000");
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_1_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_2_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_3_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_4_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_5_itm
            <= '0';
        T_LINE_t_7_0_sva <= STD_LOGIC_VECTOR'( "00000000");
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0
            <= STD_LOGIC_VECTOR'( "00000000");
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
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
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= STD_LOGIC_VECTOR'( "000000000");
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= '0';
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= '0';
      ELSIF ( core_wen = '1' ) THEN
        for_stage_0 <= NOT((NOT(for_stage_0 AND (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1
            OR for_acc_itm_6_1))) AND (fsm_output(1)));
        exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
            <= NOT(((NOT for_acc_itm_6_1) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1)
            AND (fsm_output(1)));
        for_stage_0_2 <= for_stage_0 AND (fsm_output(1));
        for_stage_0_3 <= for_stage_0_2 AND (fsm_output(1));
        for_stage_0_4 <= for_stage_0_3 AND (fsm_output(1));
        for_stage_0_5 <= for_stage_0_4 AND (fsm_output(1));
        for_stage_0_6 <= for_stage_0_5 AND (fsm_output(1));
        reg_sin_out_1_rsc_cgo_cse <= or_252_rmff;
        reg_acc_rsci_irdy_core_psct_cse <= (T_LINE_slc_T_LINE_acc_6_itm AND (fsm_output(19)))
            OR (and_dcpl_71 AND (fsm_output(1))) OR ((z_out_10(8)) AND (fsm_output(20)));
        reg_y2_rsci_ivld_core_psct_cse <= (NOT (z_out_10(8))) AND (fsm_output(20));
        T_LINE_if_if_dividend1_mul_cmp_b <= MUX1HOT_v_27_4_2(cos_out_1_rsci_data_out_d_oreg,
            sin_out_1_rsci_data_out_d_oreg, T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva,
            T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        for_acc_2_itm_1_20_7 <= MUX_v_14_2_2(STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(for_acc_7_nl),
            14)), (STD_LOGIC_VECTOR'( "00000000") & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl),
            or_266_nl);
        threshold_23_8_lpi_3 <= MUX1HOT_v_16_3_2(STD_LOGIC_VECTOR'( "0000000101000100"),
            acc_rsci_idat_mxwt, threshold_23_8_lpi_3, STD_LOGIC_VECTOR'( or_270_nl
            & and_382_cse & or_272_nl));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_1_itm
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_nl,
            fsm_output(18));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_2_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_1_itm,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_1_nl,
            fsm_output(18));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_3_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_2_itm,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_2_nl,
            fsm_output(18));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_4_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_3_itm,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_3_nl,
            fsm_output(18));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_5_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_4_itm,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_4_nl,
            fsm_output(18));
        T_LINE_t_7_0_sva <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), T_LINE_t_mux1h_3_nl,
            nor_57_nl);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_4_nl
            AND (NOT or_tmp_217);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0
            <= MUX1HOT_v_8_3_2((R_LINE_r_10_0_sva(7 DOWNTO 0)), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(7
            DOWNTO 0)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0,
            STD_LOGIC_VECTOR'( (fsm_output(1)) & (fsm_output(12)) & or_dcpl_77));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_18_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_24_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_66_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
            AND (NOT or_tmp_211);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(19)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(19)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(19)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(19)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(18)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(18)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(18)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(18)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(17)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(17)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(17)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(17)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(16)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(16)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(16)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(16)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(15)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(15)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(15)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(15)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(14)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(14)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(14)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(14)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(13)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(13)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(13)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(13)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(12)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(12)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(12)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(12)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(11)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(11)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(11)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(11)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(10)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(10)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(10)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(10)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(9)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(9)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(9)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(9)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(8)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(8)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(8)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(8)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(7)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(7)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(7)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(7)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(6)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(6)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(6)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(6)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(5)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(5)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(5)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(5)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(4)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(4)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(4)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(4)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(3)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(3)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(3)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(3)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(2)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(2)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(2)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(2)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
            <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(1)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(1)),
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(1)),
            (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(1)),
            STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
            & or_tmp_217 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= MUX_v_9_2_2((z_out_5(9 DOWNTO
            1)), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(z_out_10(7 DOWNTO 0)),9)), T_LINE_if_if_dividend1_or_nl);
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= MUX1HOT_s_1_3_2((z_out_5(0)),
            y1_t_18_lpi_3, y2_t_18_lpi_3, STD_LOGIC_VECTOR'( T_LINE_if_if_dividend1_or_1_nl
            & or_tmp_140 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= MUX1HOT_s_1_4_2(x1_t_17_lpi_3,
            y1_t_17_lpi_3, x2_t_17_lpi_3, y2_t_17_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= MUX1HOT_s_1_4_2(x1_t_16_lpi_3,
            y1_t_16_lpi_3, x2_t_16_lpi_3, y2_t_16_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= MUX1HOT_s_1_4_2(x1_t_15_lpi_3,
            y1_t_15_lpi_3, x2_t_15_lpi_3, y2_t_15_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= MUX1HOT_s_1_4_2(x1_t_14_lpi_3,
            y1_t_14_lpi_3, x2_t_14_lpi_3, y2_t_14_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= MUX1HOT_s_1_4_2(x1_t_13_lpi_3,
            y1_t_13_lpi_3, x2_t_13_lpi_3, y2_t_13_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= MUX1HOT_s_1_4_2(x1_t_12_lpi_3,
            y1_t_12_lpi_3, x2_t_12_lpi_3, y2_t_12_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= MUX1HOT_s_1_4_2(x1_t_11_lpi_3,
            y1_t_11_lpi_3, x2_t_11_lpi_3, y2_t_11_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
        reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= MUX1HOT_s_1_4_2(x1_t_10_lpi_3,
            y1_t_10_lpi_3, x2_t_10_lpi_3, y2_t_10_lpi_3, STD_LOGIC_VECTOR'( or_tmp_139
            & or_tmp_140 & or_tmp_141 & or_tmp_142));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( (core_wen AND for_stage_0 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1)
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2
            <= z_out_10(4 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_sva_3_1
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_sva_3_1
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_1_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_sva_3_1
            <= acc_nl(33 DOWNTO 1);
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_sva_3_1
            <= acc_1_nl(33 DOWNTO 1);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( (mux_nl AND for_stage_0_4 AND (NOT for_asn_7_itm_3) AND core_wen AND
          ((NOT for_stage_0_3) OR for_asn_7_itm_2)) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_42_17_lpi_2_4_0_mx0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_atan_pi_2mi_return_1_69_38_sva_3 <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( (core_wen AND (and_dcpl_21 OR (NOT for_asn_7_itm_2)) AND for_stage_0_3)
          = '1' ) THEN
        ac_math_atan_pi_2mi_return_1_69_38_sva_3 <= ac_math_atan_pi_2mi_return_1_69_38_sva_2;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_2_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_sva_3_1;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_sva_3_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_3
            <= STD_LOGIC_VECTOR'( "00000");
        reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse
            <= '0';
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_and_1_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_3
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_2;
        reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_5_itm_3_cse
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_atan_pi_2mi_return_1_69_38_sva_2 <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( (core_wen AND or_dcpl_34 AND for_stage_0_2) = '1' ) THEN
        ac_math_atan_pi_2mi_return_1_69_38_sva_2 <= ac_math_atan_pi_2mi_return_1_69_38_sva_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_2
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( (core_wen AND (or_dcpl_34 OR (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_1_itm))
          AND for_stage_0_2) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_3
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000");
      ELSIF ( (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_cse
          AND (and_dcpl_11 OR (NOT for_asn_7_itm_3)) AND for_stage_0_4) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_3
            <= MUX_v_35_2_2(z_out_3, (z_out_4(34 DOWNTO 0)), and_206_nl);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
            <= '0';
      ELSIF ( (core_wen AND (NOT or_dcpl_84)) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl,
            or_dcpl_83);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
            <= '0';
      ELSIF ( (core_wen AND (NOT(((NOT T_LINE_if_land_lpi_3_dfm) AND or_574_cse)
          OR (T_LINE_if_land_lpi_3_dfm AND ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c)
          OR or_tmp_163))) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
            <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_1_rgt);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x1_t_10_lpi_3 <= '0';
        x1_t_11_lpi_3 <= '0';
        x1_t_12_lpi_3 <= '0';
        x1_t_13_lpi_3 <= '0';
        x1_t_14_lpi_3 <= '0';
        x1_t_15_lpi_3 <= '0';
        x1_t_16_lpi_3 <= '0';
        x1_t_17_lpi_3 <= '0';
        y1_t_10_lpi_3 <= '0';
        y1_t_11_lpi_3 <= '0';
        y1_t_12_lpi_3 <= '0';
        y1_t_13_lpi_3 <= '0';
        y1_t_14_lpi_3 <= '0';
        y1_t_15_lpi_3 <= '0';
        y1_t_16_lpi_3 <= '0';
        y1_t_17_lpi_3 <= '0';
        y1_t_18_lpi_3 <= '0';
      ELSIF ( x1_t_and_2_cse = '1' ) THEN
        x1_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_11_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_10_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x1_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_9_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        y1_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_10_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_9_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_8_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y1_t_18_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_7_nl
            AND T_LINE_if_land_lpi_3_dfm;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x2_t_10_lpi_3 <= '0';
        x2_t_11_lpi_3 <= '0';
        x2_t_12_lpi_3 <= '0';
        x2_t_13_lpi_3 <= '0';
        x2_t_14_lpi_3 <= '0';
        x2_t_15_lpi_3 <= '0';
        x2_t_16_lpi_3 <= '0';
        x2_t_17_lpi_3 <= '0';
        y2_t_10_lpi_3 <= '0';
        y2_t_11_lpi_3 <= '0';
        y2_t_12_lpi_3 <= '0';
        y2_t_13_lpi_3 <= '0';
        y2_t_14_lpi_3 <= '0';
        y2_t_15_lpi_3 <= '0';
        y2_t_16_lpi_3 <= '0';
        y2_t_17_lpi_3 <= '0';
        y2_t_18_lpi_3 <= '0';
      ELSIF ( x2_t_and_3_cse = '1' ) THEN
        x2_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_11_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_10_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        x2_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_9_nl
            AND (NOT T_LINE_if_land_lpi_3_dfm);
        y2_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_10_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_9_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_8_nl
            AND T_LINE_if_land_lpi_3_dfm;
        y2_t_18_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_7_nl
            AND T_LINE_if_land_lpi_3_dfm;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_slc_T_LINE_acc_6_itm <= '0';
      ELSIF ( (core_wen AND ((fsm_output(17)) OR (fsm_output(1)))) = '1' ) THEN
        T_LINE_slc_T_LINE_acc_6_itm <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1,
            (z_out_6(6)), fsm_output(17));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
            <= '0';
      ELSIF ( (core_wen AND (or_tmp_211 OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_rgt
          OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_rgt))
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
            <= MUX1HOT_s_1_3_2(T_LINE_if_aelse_T_LINE_if_aelse_and_6_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1,
            STD_LOGIC_VECTOR'( or_tmp_211 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_rgt
            & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_rgt));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        y1_t_25_19_lpi_3 <= STD_LOGIC_VECTOR'( "0000000");
      ELSIF ( (mux_18_nl AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
          AND core_wen) = '1' ) THEN
        y1_t_25_19_lpi_3 <= MUX_v_7_2_2(z_out_2, T_LINE_if_T_LINE_if_and_17_nl, or_tmp_164);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        y2_t_25_19_lpi_3 <= STD_LOGIC_VECTOR'( "0000000");
      ELSIF ( (core_wen AND y2_t_or_itm AND (fsm_output(18))) = '1' ) THEN
        y2_t_25_19_lpi_3 <= MUX_v_7_2_2(z_out_2, STD_LOGIC_VECTOR'( "0110110"), y2_t_and_1_nl);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x1_t_26_18_lpi_3 <= STD_LOGIC_VECTOR'( "000000000");
      ELSIF ( (((fsm_output(4)) OR (fsm_output(12)) OR ((fsm_output(16)) AND T_LINE_if_land_lpi_3_dfm))
          AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
          AND core_wen) = '1' ) THEN
        x1_t_26_18_lpi_3 <= MUX1HOT_v_9_3_2(STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(operator_28_13_true_AC_TRN_AC_WRAP_acc_2_nl),
            9)), (z_out_5(8 DOWNTO 0)), T_LINE_if_T_LINE_if_and_nl, STD_LOGIC_VECTOR'(
            and_563_nl & y1_t_and_1_nl & or_tmp_164));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        x2_t_26_18_lpi_3 <= STD_LOGIC_VECTOR'( "000000000");
      ELSIF ( (core_wen AND y2_t_or_itm AND (fsm_output(19))) = '1' ) THEN
        x2_t_26_18_lpi_3 <= MUX_v_9_2_2(STD_LOGIC_VECTOR'( "010100010"), (z_out_3(8
            DOWNTO 0)), x2_t_and_1_nl);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        R_LINE_r_10_0_sva <= STD_LOGIC_VECTOR'( "00000000000");
      ELSIF ( (core_wen AND ((fsm_output(20)) OR (fsm_output(1)))) = '1' ) THEN
        R_LINE_r_10_0_sva <= MUX_v_11_2_2((STD_LOGIC_VECTOR'( "000") & R_LINE_r_R_LINE_r_and_nl),
            z_out_6, fsm_output(20));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0
            <= STD_LOGIC_VECTOR'( "000000000");
      ELSIF ( (core_wen AND (NOT or_dcpl_77)) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0
            <= MUX_v_9_2_2((STD_LOGIC_VECTOR'( "00") & (NOT (for_t_7_0_sva_mx1(6
            DOWNTO 0)))), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(8
            DOWNTO 0)), fsm_output(12));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_1_reg
            <= STD_LOGIC_VECTOR'( "000000");
      ELSIF ( ((mux_19_cse OR (fsm_output(7)) OR (fsm_output(13)) OR (fsm_output(1)))
          AND core_wen) = '1' ) THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_1_reg
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_and_5_rgt(26
            DOWNTO 21);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg
            <= STD_LOGIC_VECTOR'( "000000000000000000000");
      ELSIF ( ((mux_19_cse OR ((fsm_output(1)) AND (NOT for_asn_7_itm_3) AND for_stage_0_4)
          OR (fsm_output(7)) OR (fsm_output(13))) AND core_wen) = '1' ) THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_and_5_rgt(20
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_26_21
            <= STD_LOGIC_VECTOR'( "000000");
      ELSIF ( (and_1012_cse AND core_wen) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_26_21
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_rgt(26
            DOWNTO 21);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_20_0
            <= STD_LOGIC_VECTOR'( "000000000000000000000");
      ELSIF ( (((and_1012_cse AND or_564_cse) OR (fsm_output(5)) OR (fsm_output(11)))
          AND core_wen) = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_20_0
            <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_rgt(20
            DOWNTO 0);
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
            <= STD_LOGIC_VECTOR'( "00");
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2
            <= STD_LOGIC_VECTOR'( "000000000000000000000000");
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_ssc
          = '1' ) THEN
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_and_nl,
            ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_nl,
            fsm_output(10));
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
            <= MUX_v_2_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_6_itm_25_0(25
            DOWNTO 24)), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_8_itm_25_0(25
            DOWNTO 24)), fsm_output(10));
        reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2
            <= MUX1HOT_v_24_3_2(z_out_8, (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_6_itm_25_0(23
            DOWNTO 0)), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_mux1h_8_itm_25_0(23
            DOWNTO 0)), STD_LOGIC_VECTOR'( (fsm_output(1)) & (fsm_output(4)) & (fsm_output(10))));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva <= STD_LOGIC_VECTOR'( "000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_104 OR (fsm_output(14)) OR (fsm_output(10))
          OR (fsm_output(7)) OR or_dcpl_91))) = '1' ) THEN
        T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva <= sin_out_1_rsci_data_out_d_oreg;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva <= STD_LOGIC_VECTOR'(
            "000000000000000000000000000");
      ELSIF ( (core_wen AND (NOT(or_dcpl_104 OR (fsm_output(15)) OR (fsm_output(10))
          OR (fsm_output(7)) OR or_dcpl_91))) = '1' ) THEN
        T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva <= cos_out_1_rsci_data_out_d_oreg;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_8_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_7_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_6_itm
            <= '0';
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_itm
            <= '0';
      ELSIF ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_and_cse
          = '1' ) THEN
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_8_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_24_nl,
            (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_7_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_25_nl,
            (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_6_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_26_nl,
            (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
        ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_itm
            <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_28_nl,
            (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
      END IF;
    END IF;
  END PROCESS;
  for_acc_7_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED((NOT
      for_t_7_0_sva_mx1) & STD_LOGIC_VECTOR'( "1111") & (NOT (for_t_7_0_sva_mx1(7)))),
      13), 14) + CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(NOT for_t_7_0_sva_mx1), 8),
      14), 14));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      <= MUX_v_6_2_2(STD_LOGIC_VECTOR'("000000"), (z_out_6(5 DOWNTO 0)), or_tmp_217);
  or_266_nl <= or_dcpl_77 OR CONV_SL_1_1(fsm_output(9 DOWNTO 7)/=STD_LOGIC_VECTOR'("000"));
  or_270_nl <= (fsm_output(0)) OR (fsm_output(21));
  or_272_nl <= (and_dcpl_79 AND (NOT (fsm_output(2)))) OR (operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1
      AND (fsm_output(2)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(15)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_20_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(14)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_1_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_20_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_21_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(13)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_2_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_21_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_22_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(12)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_3_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_22_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_23_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(11)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_if_1_mux_4_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_23_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  nor_38_nl <= NOT((NOT and_dcpl_79) OR (fsm_output(20)) OR (fsm_output(16)) OR (fsm_output(1)));
  T_LINE_t_mux1h_3_nl <= MUX1HOT_v_8_3_2(for_t_7_0_sva, z_out_7, T_LINE_t_7_0_sva,
      STD_LOGIC_VECTOR'( (fsm_output(1)) & (fsm_output(16)) & nor_38_nl));
  nor_57_nl <= NOT(((NOT for_stage_0_2) AND (fsm_output(1))) OR (fsm_output(20)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_4_nl
      <= MUX1HOT_s_1_5_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0(0)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0(0)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0(0)),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0(0)),
      STD_LOGIC_VECTOR'( (fsm_output(1)) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27,
      (z_out_4(27)), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_1_rgt);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
      <= MUX_s_1_2_2((NOT (z_out_4(27))), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_18_nl
      <= MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_24_nl
      <= MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_66_nl
      <= MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva,
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  T_LINE_if_if_dividend1_or_nl <= or_tmp_140 OR or_tmp_142;
  T_LINE_if_if_dividend1_or_1_nl <= or_tmp_139 OR or_tmp_141;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_2_nl
      <= nand_12_cse OR or_tmp_399;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_48_nl
      <= MUX_v_33_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1,
      (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1),
      or_tmp_399);
  acc_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_32_30
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_26_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_23_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_20_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_9_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_6_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_2_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_conc_25_0
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_2_nl)
      + UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_48_nl
      & '1'), 34));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nand_1_nl
      <= NOT(or_tmp_399 AND nand_12_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_49_nl
      <= MUX_v_33_2_2((NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1,
      or_tmp_399);
  acc_1_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_32_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_26
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_23
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_20
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_19
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_9
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_6
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_2
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_conc_33_0
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nand_1_nl)
      + UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_49_nl
      & '1'), 34));
  nor_55_nl <= NOT(exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
      OR (NOT for_stage_0));
  nand_9_nl <= NOT(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
      AND ((NOT for_asn_7_itm_1) OR exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
      OR (NOT for_stage_0)));
  mux_nl <= MUX_s_1_2_2(nor_55_nl, nand_9_nl, for_stage_0_2);
  and_206_nl <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1)
      AND for_stage_0_4;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl
      <= NOT(CONV_SL_1_1(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0/=STD_LOGIC_VECTOR'("0000000000000000000000000000")));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000
      <= reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      OR CONV_SL_1_1(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1/=STD_LOGIC_VECTOR'("00"))
      OR CONV_SL_1_1(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2/=STD_LOGIC_VECTOR'("000000000000000000000000"));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_11_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_11_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_10_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_10_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_9_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_7_0(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_9_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_10_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_10_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_9_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_9_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_8_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_8_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_8_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_7_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(8)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_7_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_7_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_11_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_11_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_10_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_10_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_9_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_9_nl,
      (NOT (T_LINE_if_else_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(0)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(1)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(2)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(3)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_10_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_9_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_8_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(8)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_7_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  T_LINE_if_aelse_T_LINE_if_aelse_and_6_nl <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
      AND or_dcpl_11;
  T_LINE_if_T_LINE_if_and_17_nl <= MUX_v_7_2_2(STD_LOGIC_VECTOR'("0000000"), y1_t_25_19_lpi_3,
      T_LINE_if_land_lpi_3_dfm);
  mux_18_nl <= MUX_s_1_2_2((fsm_output(12)), (NOT T_LINE_if_land_lpi_3_dfm), fsm_output(16));
  y2_t_and_1_nl <= T_LINE_T_LINE_nor_3 AND (fsm_output(18));
  operator_28_13_true_AC_TRN_AC_WRAP_acc_2_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(R_LINE_r_10_0_sva(10
      DOWNTO 2)) + UNSIGNED'( "100011011"), 9));
  T_LINE_if_aelse_not_39_nl <= NOT T_LINE_if_land_lpi_3_dfm;
  T_LINE_if_T_LINE_if_and_nl <= MUX_v_9_2_2(STD_LOGIC_VECTOR'("000000000"), x1_t_26_18_lpi_3,
      T_LINE_if_aelse_not_39_nl);
  and_563_nl <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
      AND (fsm_output(4));
  y1_t_and_1_nl <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_26_svs_1)
      AND (fsm_output(12));
  x2_t_and_1_nl <= T_LINE_T_LINE_nor_3 AND (fsm_output(19));
  R_LINE_r_R_LINE_r_and_nl <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), T_LINE_t_7_0_sva,
      for_stage_0_3);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_and_nl
      <= (z_out_9(26)) AND (NOT(asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      OR and_224_ssc));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_nl
      <= (z_out_9(26)) AND (NOT(and_226_ssc OR asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_24_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(8)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_25_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(9)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_26_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(10)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_28_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(16)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  nand_12_cse <= NOT((NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1)
      AND (fsm_output(1)));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_44_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(15)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_44_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_45_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(15)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_45_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_7_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl,
      fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_46_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(14)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_46_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_47_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(14)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_47_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_8_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl,
      fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_48_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(13)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_48_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_49_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(13)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_49_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_9_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl,
      fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_50_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(12)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_50_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_51_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(12)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_51_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_10_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl,
      fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_52_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(11)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_52_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_53_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(11)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_53_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_11_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl,
      fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_54_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(10)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_54_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_55_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(10)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_55_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_12_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl,
      fsm_output(18));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_56_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(9)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_56_nl,
      (NOT (T_LINE_if_if_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_57_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_2(9)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_57_nl,
      (NOT (T_LINE_if_if_dividend2_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_mux_13_nl <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl,
      fsm_output(18));
  z_out_2 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED'( operator_20_15_true_AC_TRN_AC_WRAP_mux_7_nl
      & operator_20_15_true_AC_TRN_AC_WRAP_mux_8_nl & operator_20_15_true_AC_TRN_AC_WRAP_mux_9_nl
      & operator_20_15_true_AC_TRN_AC_WRAP_mux_10_nl & operator_20_15_true_AC_TRN_AC_WRAP_mux_11_nl
      & operator_20_15_true_AC_TRN_AC_WRAP_mux_12_nl & operator_20_15_true_AC_TRN_AC_WRAP_mux_13_nl)
      + UNSIGNED'( "0011011"), 7));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_6_nl
      <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_34_32_1,
      STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_itm,
      1),3)), fsm_output(19));
  not_424_nl <= NOT or_523_cse;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_6_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_6_nl,
      not_424_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_7_nl
      <= MUX_v_24_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1(31
      DOWNTO 8)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_itm,
      1),24)), fsm_output(19));
  not_425_nl <= NOT or_523_cse;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_7_nl
      <= MUX_v_24_2_2(STD_LOGIC_VECTOR'("000000000000000000000000"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_7_nl,
      not_425_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_8_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1(7)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_1_itm,
      fsm_output(19));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_8_nl
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_8_nl
      AND (NOT or_523_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_9_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1(6)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_2_itm,
      fsm_output(19));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_9_nl
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_9_nl
      AND (NOT or_523_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_10_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1(5)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_3_itm,
      fsm_output(19));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_10_nl
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_10_nl
      AND (NOT or_523_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_11_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1(4)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_4_itm,
      fsm_output(19));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_11_nl
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_11_nl
      AND (NOT or_523_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_2_nl
      <= MUX1HOT_v_4_3_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1(3
      DOWNTO 0)), STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_5_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_6_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_7_itm
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_8_itm),
      (z_out_6(5 DOWNTO 2)), STD_LOGIC_VECTOR'( (fsm_output(1)) & (fsm_output(19))
      & or_523_cse));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_nl
      <= (NOT((fsm_output(19)) OR or_523_cse)) OR (fsm_output(1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_nl
      <= (NOT (fsm_output(19))) OR (fsm_output(1)) OR or_523_cse;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_3_nl
      <= MUX1HOT_v_32_3_2((NOT ac_math_atan_pi_2mi_return_1_69_38_sva_3), STD_LOGIC_VECTOR'(
      "00000000000000000000000001010001"), STD_LOGIC_VECTOR'( "11111111111111111111111111110101"),
      STD_LOGIC_VECTOR'( (fsm_output(1)) & (fsm_output(19)) & or_523_cse));
  acc_3_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_6_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_7_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_8_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_9_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_10_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_11_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_2_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_nl)
      + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_nl
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_3_nl
      & '1'), 34), 36), 36));
  z_out_3 <= acc_3_nl(35 DOWNTO 1);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      <= (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_26_21(5))
      AND (NOT ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_4_nl
      <= MUX_v_27_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_26_21
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva_20_0),
      (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_2_nl
      <= MUX1HOT_v_44_4_2((STD_LOGIC_VECTOR'( "000000000000") & ac_math_atan_pi_2mi_return_1_69_38_sva_3),
      STD_LOGIC_VECTOR'( "00000000000000000000000000000000000000000001"), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_4_nl),44)),
      (NOT T_LINE_if_if_dividend1_mul_cmp_z_oreg), STD_LOGIC_VECTOR'( (fsm_output(1))
      & or_dcpl_94 & or_523_cse & or_tmp_410));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      <= MUX1HOT_v_40_4_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_34_32_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_2_31_0_1),40)),
      (STD_LOGIC_VECTOR'( "0000000000000") & (NOT reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd)
      & (NOT reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1)
      & (NOT reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_2)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_1_reg
      & reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_2_reg
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva),40)),
      (x1_t_26_18_lpi_3 & (R_LINE_r_10_0_sva(1 DOWNTO 0)) & STD_LOGIC_VECTOR'( "00000000000000000000000000001")),
      STD_LOGIC_VECTOR'( (fsm_output(1)) & or_dcpl_94 & or_523_cse & or_tmp_410));
  z_out_4 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_2_nl)
      + CONV_UNSIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl),
      44), 44));
  operator_20_15_true_AC_TRN_AC_WRAP_2_or_1_cse <= (fsm_output(4)) OR (fsm_output(10));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_29_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(16)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_29_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_58_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(15)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_58_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_59_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(14)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_59_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_60_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(13)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_60_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_61_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(12)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_61_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_62_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(11)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_62_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_30_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(10)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_30_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_63_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(9)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_63_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_64_nl
      <= MUX_s_1_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_sva_2(8)),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_64_nl,
      (NOT (T_LINE_if_else_dividend1_sva(19))), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_1_nl <= MUX1HOT_v_9_3_2(STD_LOGIC_VECTOR'(
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl),
      x1_t_26_18_lpi_3, x2_t_26_18_lpi_3, STD_LOGIC_VECTOR'( (fsm_output(12)) & (fsm_output(4))
      & (fsm_output(10))));
  operator_20_15_true_AC_TRN_AC_WRAP_2_mux_1_nl <= MUX_v_7_2_2(STD_LOGIC_VECTOR'(
      "0101000"), STD_LOGIC_VECTOR'( "1010111"), operator_20_15_true_AC_TRN_AC_WRAP_2_or_1_cse);
  z_out_5 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(SIGNED(operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_1_nl),
      10) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(operator_20_15_true_AC_TRN_AC_WRAP_2_mux_1_nl
      & '1'), 8), 10), 10));
  T_LINE_mux1h_4_nl <= MUX1HOT_v_11_3_2(STD_LOGIC_VECTOR'( "11111010011"), R_LINE_r_10_0_sva,
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(for_acc_2_itm_1_20_7(5 DOWNTO 0)),11)),
      STD_LOGIC_VECTOR'( (fsm_output(17)) & (fsm_output(20)) & or_523_cse));
  T_LINE_or_3_nl <= (fsm_output(20)) OR or_523_cse;
  T_LINE_T_LINE_mux_2_nl <= MUX_v_6_2_2((T_LINE_t_7_0_sva(7 DOWNTO 2)), STD_LOGIC_VECTOR'(
      "000001"), T_LINE_or_3_nl);
  z_out_6 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(T_LINE_mux1h_4_nl) + CONV_UNSIGNED(UNSIGNED(T_LINE_T_LINE_mux_2_nl),
      11), 11));
  T_LINE_mux_2_nl <= MUX_v_8_2_2(T_LINE_t_7_0_sva, for_t_7_0_sva_mx1, fsm_output(1));
  z_out_7 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(T_LINE_mux_2_nl) + UNSIGNED'(
      "00000001"), 8));
  for_not_10_nl <= NOT (fsm_output(3));
  for_for_and_2_nl <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), for_t_7_0_sva, for_not_10_nl);
  for_not_11_nl <= NOT (fsm_output(3));
  for_for_and_3_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (for_t_7_0_sva(7 DOWNTO
      6)), for_not_11_nl);
  for_mux_7_nl <= MUX_v_6_2_2((for_t_7_0_sva(5 DOWNTO 0)), (T_LINE_t_7_0_sva(7 DOWNTO
      2)), fsm_output(3));
  for_mux_8_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'( "01"), (T_LINE_t_7_0_sva(1 DOWNTO
      0)), fsm_output(3));
  for_acc_8_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(for_acc_2_itm_1_20_7) + SIGNED'(
      "01111100000001"), 14));
  for_for_or_1_nl <= MUX_v_14_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(for_acc_8_nl),
      14)), STD_LOGIC_VECTOR'("11111111111111"), (fsm_output(3)));
  for_mux_9_nl <= MUX_v_7_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_sva_8_0(6
      DOWNTO 0)), STD_LOGIC_VECTOR'( "1010011"), fsm_output(3));
  z_out_8 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(for_for_and_2_nl & STD_LOGIC_VECTOR'(
      "000000") & for_for_and_3_nl & for_mux_7_nl & for_mux_8_nl) + CONV_UNSIGNED(CONV_SIGNED(SIGNED('1'
      & for_for_or_1_nl & for_mux_9_nl), 22), 24), 24));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_2_nl
      <= MUX1HOT_v_7_4_2((cos_out_1_rsci_data_out_d_oreg(25 DOWNTO 19)), (sin_out_1_rsci_data_out_d_oreg(25
      DOWNTO 19)), (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(25 DOWNTO 19)), (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(25
      DOWNTO 19)), STD_LOGIC_VECTOR'( and_971_ssc & and_973_ssc & and_975_ssc & and_977_ssc));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_or_1_nl
      <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      OR ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_nor_1_nl
      <= NOT(MUX_v_7_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_2_nl,
      STD_LOGIC_VECTOR'("1111111"), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_or_1_nl));
  ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_3_nl
      <= MUX1HOT_v_19_8_2((NOT (T_LINE_if_if_dividend1_sva(18 DOWNTO 0))), (NOT (T_LINE_if_else_dividend1_sva(18
      DOWNTO 0))), (NOT (T_LINE_if_if_dividend2_sva(18 DOWNTO 0))), (NOT (T_LINE_if_else_dividend2_sva(18
      DOWNTO 0))), (NOT (cos_out_1_rsci_data_out_d_oreg(18 DOWNTO 0))), (NOT (sin_out_1_rsci_data_out_d_oreg(18
      DOWNTO 0))), (NOT (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva(18 DOWNTO 0))),
      (NOT (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva(18 DOWNTO 0))),
      STD_LOGIC_VECTOR'( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      & and_971_ssc & and_973_ssc & and_975_ssc & and_977_ssc));
  z_out_9 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_nor_1_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_3_nl),
      26), 27) + UNSIGNED'( "000000000000000000000000001"), 27));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_13_nl
      <= MUX1HOT_v_9_5_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_lpi_2_dfm_1_1),9)),
      (STD_LOGIC_VECTOR'( "1111") & (NOT (T_LINE_t_7_0_sva(7 DOWNTO 3)))), ('0' &
      (z_out_6(10 DOWNTO 3))), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(y1_t_25_19_lpi_3),9)),
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(y2_t_25_19_lpi_3),9)), STD_LOGIC_VECTOR'(
      (fsm_output(1)) & (fsm_output(3)) & (fsm_output(20)) & (fsm_output(4)) & (fsm_output(10))));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_14_nl
      <= MUX1HOT_v_9_4_2(STD_LOGIC_VECTOR'( "000000001"), STD_LOGIC_VECTOR'( "000010001"),
      STD_LOGIC_VECTOR'( "100011011"), STD_LOGIC_VECTOR'( "111100101"), STD_LOGIC_VECTOR'(
      (fsm_output(1)) & (fsm_output(3)) & (fsm_output(20)) & operator_20_15_true_AC_TRN_AC_WRAP_2_or_1_cse));
  z_out_10 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_13_nl)
      + UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_14_nl),
      9));
END v25;

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
    acc_tmp_rsci_addr_d : OUT STD_LOGIC_VECTOR (37 DOWNTO 0);
    acc_tmp_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    acc_tmp_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    acc_tmp_rsci_data_out_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
    acc_tmp_rsci_en_d : OUT STD_LOGIC;
    cos_out_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    cos_out_rsci_addr_d : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    cos_out_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    cos_out_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    cos_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
    cos_out_rsci_en_d : OUT STD_LOGIC;
    sin_out_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
    sin_out_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    sin_out_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
    sin_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0)
  );
END houghTransform_core;

ARCHITECTURE v25 OF houghTransform_core IS
  -- Default Constants

  -- Interconnect Declarations
  SIGNAL core_wen : STD_LOGIC;
  SIGNAL core_wten : STD_LOGIC;
  SIGNAL data_in_rsci_wen_comp : STD_LOGIC;
  SIGNAL data_in_rsci_idat_mxwt : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL acc_rsci_wen_comp : STD_LOGIC;
  SIGNAL acc_rsci_idat : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_out_d_oreg : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL cos_out_rsci_data_out_d_oreg : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL sin_out_rsci_data_out_d_oreg : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL fsm_output : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL HROW_equal_tmp : STD_LOGIC;
  SIGNAL HCOL_equal_tmp : STD_LOGIC;
  SIGNAL for_for_nor_tmp : STD_LOGIC;
  SIGNAL and_dcpl : STD_LOGIC;
  SIGNAL and_dcpl_25 : STD_LOGIC;
  SIGNAL and_dcpl_26 : STD_LOGIC;
  SIGNAL and_dcpl_33 : STD_LOGIC;
  SIGNAL and_dcpl_35 : STD_LOGIC;
  SIGNAL or_dcpl_36 : STD_LOGIC;
  SIGNAL and_dcpl_43 : STD_LOGIC;
  SIGNAL or_dcpl_38 : STD_LOGIC;
  SIGNAL or_dcpl_42 : STD_LOGIC;
  SIGNAL and_dcpl_53 : STD_LOGIC;
  SIGNAL and_dcpl_60 : STD_LOGIC;
  SIGNAL or_tmp_59 : STD_LOGIC;
  SIGNAL or_tmp_81 : STD_LOGIC;
  SIGNAL and_36_cse : STD_LOGIC;
  SIGNAL and_87_cse : STD_LOGIC;
  SIGNAL and_88_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1
      : STD_LOGIC;
  SIGNAL HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_7_itm_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2
      : STD_LOGIC;
  SIGNAL HACC_stage_0_7 : STD_LOGIC;
  SIGNAL for_1_stage_0 : STD_LOGIC;
  SIGNAL for_1_stage_0_4 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_2
      : STD_LOGIC;
  SIGNAL for_1_asn_7_itm_2 : STD_LOGIC;
  SIGNAL for_1_stage_0_2 : STD_LOGIC;
  SIGNAL for_1_stage_0_3 : STD_LOGIC;
  SIGNAL exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
      : STD_LOGIC;
  SIGNAL for_1_asn_7_itm_1 : STD_LOGIC;
  SIGNAL HACC_stage_0_3 : STD_LOGIC;
  SIGNAL HACC_stage_0 : STD_LOGIC;
  SIGNAL HACC_stage_0_6 : STD_LOGIC;
  SIGNAL HACC_stage_0_5 : STD_LOGIC;
  SIGNAL HACC_stage_0_4 : STD_LOGIC;
  SIGNAL HACC_stage_0_2 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_4
      : STD_LOGIC;
  SIGNAL for_1_stage_0_5 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_3
      : STD_LOGIC;
  SIGNAL HACC_stage_0_8 : STD_LOGIC;
  SIGNAL for_1_asn_7_itm_3 : STD_LOGIC;
  SIGNAL reg_heightIn_rsc_triosy_obj_ld_core_psct_cse : STD_LOGIC;
  SIGNAL reg_sin_out_rsc_cgo_cse : STD_LOGIC;
  SIGNAL reg_acc_rsci_ivld_core_psct_cse : STD_LOGIC;
  SIGNAL reg_data_in_rsci_irdy_core_psct_cse : STD_LOGIC;
  SIGNAL reg_acc_tmp_rsc_cgo_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_cse
      : STD_LOGIC;
  SIGNAL reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_1_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse
      : STD_LOGIC;
  SIGNAL or_72_rmff : STD_LOGIC;
  SIGNAL or_71_rmff : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_rmff
      : STD_LOGIC;
  SIGNAL acc_tmp_vinit_ndx_sva_mx0w1 : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL HACC_acc_4_itm_1 : STD_LOGIC_VECTOR (11 DOWNTO 0);
  SIGNAL HACC_idx_acc_2_itm_1_14_2 : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_itm :
      STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL or_tmp_123 : STD_LOGIC;
  SIGNAL z_out : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL z_out_1 : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL z_out_2 : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL z_out_3 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL z_out_4 : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL z_out_5 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL operator_11_false_io_read_widthIn_rsc_cse_sva : STD_LOGIC_VECTOR (10 DOWNTO
      0);
  SIGNAL operator_10_false_io_read_heightIn_rsc_cse_sva : STD_LOGIC_VECTOR (9 DOWNTO
      0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL HROW_y_sva : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL HACC_t_sva : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL for_1_t_7_0_sva_1_1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_2
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_3
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_return_69_38_sva_3 : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL for_1_acc_4_itm_1 : STD_LOGIC_VECTOR (23 DOWNTO 0);
  SIGNAL HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1 : STD_LOGIC_VECTOR
      (10 DOWNTO 0);
  SIGNAL HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1 : STD_LOGIC_VECTOR (12
      DOWNTO 0);
  SIGNAL HACC_idx_18_2_sva_1 : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL HACC_idx_18_2_sva_2 : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL HACC_mul_itm_1 : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL HACC_acc_5_itm_1 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL HACC_mul_1_itm_1 : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_7_2_itm_2 : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_7_2_itm_3 : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_7_2_itm_4 : STD_LOGIC_VECTOR (5 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_1_0_1_itm_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_1_0_1_itm_2 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_1_0_1_itm_3 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_1_0_1_itm_4 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_1_0_itm_5 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HACC_idx_slc_HACC_t_1_0_itm_6 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL HCOL_acc_1_itm : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL HACC_ac_fixed_cctor_sva_1_15_14 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_25_5
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_4_0
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w0
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w1
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL for_1_t_7_0_sva_mx1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_2
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5_mx0
      : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0_mx0
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL for_1_acc_5_psp_1 : STD_LOGIC_VECTOR (20 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1_1
      : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_34_32_1
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_31_0_1
      : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1
      : STD_LOGIC;
  SIGNAL HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1 : STD_LOGIC_VECTOR
      (12 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_32_30
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_29
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_26_25
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_23_22
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_20_18
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_9_8
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_6_5
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_2_1
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_0
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_32_29
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_28_27
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_26
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_25
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_24
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_23
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_22
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_21
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_20
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_19
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_18
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_17
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_16
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_15_14
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_13_11
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_10
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_9
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_8
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_7
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_6
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_5
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_4
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_3
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_2
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_0
      : STD_LOGIC;
  SIGNAL and_60_rgt : STD_LOGIC;
  SIGNAL and_61_rgt : STD_LOGIC;
  SIGNAL for_1_or_3_ssc : STD_LOGIC;
  SIGNAL for_1_and_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_cse
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_and_1_cse
      : STD_LOGIC;
  SIGNAL for_1_and_7_cse : STD_LOGIC;
  SIGNAL HACC_idx_and_1_cse : STD_LOGIC;
  SIGNAL HACC_round_r_HACC_round_r_acc_conv_2f_and_1_cse : STD_LOGIC;
  SIGNAL HACC_idx_and_8_cse : STD_LOGIC;
  SIGNAL nor_16_seb : STD_LOGIC;
  SIGNAL WRITE_i_18_0_sva_18_14 : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL WRITE_i_18_0_sva_13_11 : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL WRITE_i_18_0_sva_10_0 : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL for_1_or_6_ssc : STD_LOGIC;
  SIGNAL WRITE_i_and_1_cse : STD_LOGIC;
  SIGNAL and_355_cse : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1
      : STD_LOGIC;
  SIGNAL HACC_acc_itm_35_10 : STD_LOGIC_VECTOR (25 DOWNTO 0);
  SIGNAL WRITE_acc_itm_12_1 : STD_LOGIC;
  SIGNAL operator_8_false_acc_itm_6_1 : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      : STD_LOGIC;
  SIGNAL for_1_nor_1_seb : STD_LOGIC;
  SIGNAL nand_13_cse : STD_LOGIC;

  SIGNAL mux_6_nl : STD_LOGIC;
  SIGNAL or_181_nl : STD_LOGIC;
  SIGNAL nor_5_nl : STD_LOGIC;
  SIGNAL WRITE_i_mux1h_4_nl : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL WRITE_i_asn_WRITE_i_18_0_sva_WRITE_i_and_1_nl : STD_LOGIC_VECTOR (2 DOWNTO
      0);
  SIGNAL WRITE_i_WRITE_i_mux_nl : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL or_211_nl : STD_LOGIC;
  SIGNAL WRITE_i_mux1h_9_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL WRITE_i_WRITE_i_WRITE_i_or_nl : STD_LOGIC;
  SIGNAL WRITE_i_or_1_nl : STD_LOGIC;
  SIGNAL and_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux1h_2_nl
      : STD_LOGIC_VECTOR (34 DOWNTO 0);
  SIGNAL or_114_nl : STD_LOGIC;
  SIGNAL and_185_nl : STD_LOGIC;
  SIGNAL and_187_nl : STD_LOGIC;
  SIGNAL HACC_mul_1_nl : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL mux_nl : STD_LOGIC;
  SIGNAL or_nl : STD_LOGIC;
  SIGNAL nor_22_nl : STD_LOGIC;
  SIGNAL or_200_nl : STD_LOGIC;
  SIGNAL HACC_mux1h_5_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_2_nl
      : STD_LOGIC;
  SIGNAL T_LINE_if_aelse_T_LINE_if_aelse_and_1_nl : STD_LOGIC;
  SIGNAL and_23_nl : STD_LOGIC;
  SIGNAL HACC_mux_6_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_nl
      : STD_LOGIC;
  SIGNAL or_144_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_59_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_58_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_57_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_56_nl
      : STD_LOGIC;
  SIGNAL HACC_round_r_HACC_round_r_acc_conv_2f_or_nl : STD_LOGIC;
  SIGNAL HACC_t_mux1h_3_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL Hough_Algorithm_HW_1296_864_houghTransform_not_nl : STD_LOGIC;
  SIGNAL HACC_acc_5_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL for_1_t_for_1_t_and_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL for_1_t_mux_2_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL and_217_nl : STD_LOGIC;
  SIGNAL HACC_idx_acc_6_nl : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL HACC_idx_acc_8_nl : STD_LOGIC_VECTOR (8 DOWNTO 0);
  SIGNAL HACC_mul_nl : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_nl
      : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_3_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_50_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_69_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_47_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_76_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_8_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_57_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_43_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_6_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_59_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_39_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_4_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_60_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_35_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_32_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_53_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_nl
      : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_51_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_36_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_50_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_40_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_nl
      : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_58_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_44_nl
      : STD_LOGIC;
  SIGNAL HACC_round_r_HACC_round_r_acc_conv_2f_and_nl : STD_LOGIC;
  SIGNAL HACC_acc_nl : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_68_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_52_nl
      : STD_LOGIC;
  SIGNAL WRITE_acc_nl : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL operator_8_false_acc_nl : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl
      : STD_LOGIC_VECTOR (35 DOWNTO 0);
  SIGNAL for_mux1h_6_nl : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL WRITE_i_WRITE_i_and_nl : STD_LOGIC_VECTOR (18 DOWNTO 0);
  SIGNAL for_and_nl : STD_LOGIC;
  SIGNAL for_or_1_nl : STD_LOGIC;
  SIGNAL for_for_mux_1_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL or_95_nl : STD_LOGIC;
  SIGNAL or_93_nl : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_2_nl
      : STD_LOGIC;
  SIGNAL HACC_t_HACC_t_and_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL or_216_nl : STD_LOGIC;
  SIGNAL acc_nl : STD_LOGIC_VECTOR (33 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_2_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_48_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL acc_1_nl : STD_LOGIC_VECTOR (33 DOWNTO 0);
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_nand_1_nl
      : STD_LOGIC;
  SIGNAL ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_49_nl
      : STD_LOGIC_VECTOR (32 DOWNTO 0);
  SIGNAL HACC_idx_mux_5_nl : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL HACC_idx_acc_9_nl : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL HACC_idx_mux_6_nl : STD_LOGIC_VECTOR (12 DOWNTO 0);
  SIGNAL for_1_mux_11_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL acc_4_nl : STD_LOGIC_VECTOR (19 DOWNTO 0);
  SIGNAL for_1_for_1_and_9_nl : STD_LOGIC_VECTOR (4 DOWNTO 0);
  SIGNAL for_1_for_1_and_10_nl : STD_LOGIC;
  SIGNAL for_1_for_1_and_11_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_1_mux_12_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_1_nor_6_nl : STD_LOGIC;
  SIGNAL for_1_mux1h_33_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL for_1_or_8_nl : STD_LOGIC;
  SIGNAL for_1_or_9_nl : STD_LOGIC;
  SIGNAL for_1_for_1_or_1_nl : STD_LOGIC;
  SIGNAL for_1_for_1_for_1_nor_1_nl : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL for_1_or_10_nl : STD_LOGIC;
  SIGNAL for_1_mux1h_34_nl : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL for_1_or_11_nl : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL for_1_mux1h_35_nl : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL for_1_or_12_nl : STD_LOGIC;
  SIGNAL for_1_for_1_and_12_nl : STD_LOGIC_VECTOR (2 DOWNTO 0);
  SIGNAL for_1_nor_9_nl : STD_LOGIC;
  SIGNAL for_1_mux1h_36_nl : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      : STD_LOGIC_VECTOR (6 DOWNTO 0);
  SIGNAL ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
      : STD_LOGIC_VECTOR (31 DOWNTO 0);

  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a : STD_LOGIC_VECTOR (32 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s : STD_LOGIC_VECTOR (4 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_z : STD_LOGIC_VECTOR (32 DOWNTO
      0);

  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a : STD_LOGIC_VECTOR (32 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s : STD_LOGIC_VECTOR (4 DOWNTO
      0);
  SIGNAL operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z : STD_LOGIC_VECTOR (32 DOWNTO
      0);

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
      cos_out_rsc_cgo_iro : IN STD_LOGIC;
      cos_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      cos_out_rsci_en_d : OUT STD_LOGIC;
      sin_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      core_wen : IN STD_LOGIC;
      acc_tmp_rsc_cgo : IN STD_LOGIC;
      acc_tmp_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
      cos_out_rsc_cgo : IN STD_LOGIC;
      cos_out_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      sin_out_rsci_data_out_d_oreg : OUT STD_LOGIC_VECTOR (26 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d : STD_LOGIC_VECTOR
      (31 DOWNTO 0);
  SIGNAL houghTransform_core_wait_dp_inst_cos_out_rsci_data_out_d : STD_LOGIC_VECTOR
      (53 DOWNTO 0);
  SIGNAL houghTransform_core_wait_dp_inst_sin_out_rsci_data_out_d : STD_LOGIC_VECTOR
      (53 DOWNTO 0);
  SIGNAL houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d_oreg : STD_LOGIC_VECTOR
      (31 DOWNTO 0);
  SIGNAL houghTransform_core_wait_dp_inst_cos_out_rsci_data_out_d_oreg : STD_LOGIC_VECTOR
      (26 DOWNTO 0);
  SIGNAL houghTransform_core_wait_dp_inst_sin_out_rsci_data_out_d_oreg : STD_LOGIC_VECTOR
      (26 DOWNTO 0);

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
      fsm_output : OUT STD_LOGIC_VECTOR (11 DOWNTO 0);
      acc_tmp_vinit_C_0_tr0 : IN STD_LOGIC;
      for_1_C_0_tr0 : IN STD_LOGIC;
      HCOL_C_1_tr0 : IN STD_LOGIC;
      HACC_C_0_tr0 : IN STD_LOGIC;
      HCOL_C_2_tr0 : IN STD_LOGIC;
      HROW_C_1_tr0 : IN STD_LOGIC;
      WRITE_C_0_tr0 : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL houghTransform_core_core_fsm_inst_fsm_output : STD_LOGIC_VECTOR (11 DOWNTO
      0);
  SIGNAL houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 : STD_LOGIC;
  SIGNAL houghTransform_core_core_fsm_inst_HCOL_C_1_tr0 : STD_LOGIC;
  SIGNAL houghTransform_core_core_fsm_inst_HACC_C_0_tr0 : STD_LOGIC;

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

  FUNCTION MUX1HOT_v_11_4_2(input_3 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
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
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_11_5_2(input_4 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_3 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(10 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(4 DOWNTO 0))
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

  FUNCTION MUX1HOT_v_19_4_2(input_3 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(18 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
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

  FUNCTION MUX1HOT_v_35_4_2(input_3 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(34 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
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

  FUNCTION MUX1HOT_v_5_3_2(input_2 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(4 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(4 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(4 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_8_3_2(input_2 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(2 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(7 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(7 DOWNTO 0);

    BEGIN
      tmp := (OTHERS=>sel(0));
      result := input_0 and tmp;
      tmp := (OTHERS=>sel( 1));
      result := result or ( input_1 and tmp);
      tmp := (OTHERS=>sel( 2));
      result := result or ( input_2 and tmp);
    RETURN result;
  END;

  FUNCTION MUX1HOT_v_8_4_2(input_3 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_2 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  input_0 : STD_LOGIC_VECTOR(7 DOWNTO 0);
  sel : STD_LOGIC_VECTOR(3 DOWNTO 0))
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(7 DOWNTO 0);
    VARIABLE tmp : STD_LOGIC_VECTOR(7 DOWNTO 0);

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

  FUNCTION MUX_v_13_2_2(input_0 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(12 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(12 DOWNTO 0);

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

  FUNCTION MUX_v_17_2_2(input_0 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(16 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(16 DOWNTO 0);

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

  FUNCTION MUX_v_21_2_2(input_0 : STD_LOGIC_VECTOR(20 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(20 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(20 DOWNTO 0);

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

  FUNCTION MUX_v_36_2_2(input_0 : STD_LOGIC_VECTOR(35 DOWNTO 0);
  input_1 : STD_LOGIC_VECTOR(35 DOWNTO 0);
  sel : STD_LOGIC)
  RETURN STD_LOGIC_VECTOR IS
    VARIABLE result : STD_LOGIC_VECTOR(35 DOWNTO 0);

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
  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg : work.hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60_pkg.hough_algorithm_hw_1296_864mgc_rom_26_70_32_1_60
    PORT MAP(
      addr => ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr,
      data_out => ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out
    );
  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr <= STD_LOGIC_VECTOR(UNSIGNED'(
      "00") & UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1_1));
  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_itm <= ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_data_out;

  operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg : work.mgc_shift_comps_v5.mgc_shift_r_v5
    GENERIC MAP(
      width_a => 33,
      signd_a => 1,
      width_s => 5,
      width_z => 33
      )
    PORT MAP(
      a => operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a,
      s => operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s,
      z => operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_z
    );
  operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_32_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_26
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_23
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_20
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_19
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_9
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_6
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_2
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_0;
  operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1
      <= operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_z;

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
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_32_30
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_26_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_23_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_20_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_9_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_6_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_2_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_0;
  operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1
      <= operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_z;

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
      acc_tmp_rsc_cgo_iro => or_72_rmff,
      acc_tmp_rsci_data_out_d => houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d,
      acc_tmp_rsci_en_d => acc_tmp_rsci_en_d,
      cos_out_rsc_cgo_iro => or_71_rmff,
      cos_out_rsci_data_out_d => houghTransform_core_wait_dp_inst_cos_out_rsci_data_out_d,
      cos_out_rsci_en_d => cos_out_rsci_en_d,
      sin_out_rsci_data_out_d => houghTransform_core_wait_dp_inst_sin_out_rsci_data_out_d,
      core_wen => core_wen,
      acc_tmp_rsc_cgo => reg_acc_tmp_rsc_cgo_cse,
      acc_tmp_rsci_data_out_d_oreg => houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d_oreg,
      cos_out_rsc_cgo => reg_sin_out_rsc_cgo_cse,
      cos_out_rsci_data_out_d_oreg => houghTransform_core_wait_dp_inst_cos_out_rsci_data_out_d_oreg,
      sin_out_rsci_data_out_d_oreg => houghTransform_core_wait_dp_inst_sin_out_rsci_data_out_d_oreg
    );
  houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d <= acc_tmp_rsci_data_out_d;
  houghTransform_core_wait_dp_inst_cos_out_rsci_data_out_d <= cos_out_rsci_data_out_d;
  houghTransform_core_wait_dp_inst_sin_out_rsci_data_out_d <= sin_out_rsci_data_out_d;
  acc_tmp_rsci_data_out_d_oreg <= houghTransform_core_wait_dp_inst_acc_tmp_rsci_data_out_d_oreg;
  cos_out_rsci_data_out_d_oreg <= houghTransform_core_wait_dp_inst_cos_out_rsci_data_out_d_oreg;
  sin_out_rsci_data_out_d_oreg <= houghTransform_core_wait_dp_inst_sin_out_rsci_data_out_d_oreg;

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
      for_1_C_0_tr0 => and_dcpl_26,
      HCOL_C_1_tr0 => houghTransform_core_core_fsm_inst_HCOL_C_1_tr0,
      HACC_C_0_tr0 => houghTransform_core_core_fsm_inst_HACC_C_0_tr0,
      HCOL_C_2_tr0 => HCOL_equal_tmp,
      HROW_C_1_tr0 => HROW_equal_tmp,
      WRITE_C_0_tr0 => and_dcpl_33
    );
  fsm_output <= houghTransform_core_core_fsm_inst_fsm_output;
  houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 <= for_for_nor_tmp;
  houghTransform_core_core_fsm_inst_HCOL_C_1_tr0 <= NOT (z_out_4(8));
  houghTransform_core_core_fsm_inst_HACC_C_0_tr0 <= (NOT(HACC_stage_0_7 OR HACC_stage_0_6))
      AND (NOT(HACC_stage_0_5 OR HACC_stage_0_4)) AND (NOT(HACC_stage_0_3 OR HACC_stage_0
      OR HACC_stage_0_2));

  or_71_rmff <= ((and_dcpl_35 OR ((NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_4)
      AND for_1_stage_0_5)) AND (fsm_output(3))) OR ((z_out_4(8)) AND (fsm_output(5)))
      OR (HACC_stage_0 AND (fsm_output(6)));
  or_72_rmff <= and_87_cse OR and_88_cse OR ((HACC_stage_0_7 OR HACC_stage_0_6 OR
      HACC_stage_0_5 OR HACC_stage_0_8) AND (fsm_output(6))) OR (HACC_stage_0 AND
      (fsm_output(10)));
  and_60_rgt <= and_dcpl_43 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1;
  and_61_rgt <= and_dcpl_43 AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_cse <=
      core_wen AND ((or_dcpl_38 AND for_1_stage_0_4 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      AND (fsm_output(3))) OR (or_dcpl_38 AND for_1_stage_0_4 AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1)
      AND (fsm_output(3))));
  nor_16_seb <= NOT((fsm_output(11)) OR (fsm_output(0)) OR (fsm_output(8)) OR and_87_cse);
  WRITE_i_and_1_cse <= for_1_stage_0 AND (fsm_output(3));
  for_1_and_cse <= core_wen AND for_1_stage_0_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_cse
      <= core_wen AND for_1_stage_0;
  or_nl <= (NOT for_1_stage_0_2) OR for_1_asn_7_itm_1 OR (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1);
  nor_22_nl <= NOT((NOT for_1_stage_0_2) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1);
  or_200_nl <= (NOT for_1_stage_0) OR exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva;
  mux_nl <= MUX_s_1_2_2(or_nl, nor_22_nl, or_200_nl);
  and_355_cse <= mux_nl AND for_1_stage_0_4 AND (NOT for_1_asn_7_itm_3) AND core_wen
      AND ((NOT for_1_stage_0_3) OR for_1_asn_7_itm_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_and_1_cse
      <= core_wen AND (and_dcpl OR (NOT for_1_asn_7_itm_2) OR (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_2))
      AND for_1_stage_0_3;
  for_1_and_7_cse <= core_wen AND for_1_stage_0_2;
  HACC_idx_and_1_cse <= core_wen AND HACC_stage_0_4;
  HACC_round_r_HACC_round_r_acc_conv_2f_and_1_cse <= core_wen AND HACC_stage_0_3;
  HACC_idx_and_8_cse <= core_wen AND HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0
      <= (HACC_ac_fixed_cctor_sva_1_15_14(0)) XOR (HACC_ac_fixed_cctor_sva_1_15_14(1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs,
      or_dcpl_36);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w0
      <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_34_32_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_31_0_1)
      + CONV_SIGNED(CONV_SIGNED(SIGNED('1' & (NOT ac_math_atan_pi_2mi_return_69_38_sva_3)),
      33), 35) + SIGNED'( "00000000000000000000000000000000001"), 35));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w1
      <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_34_32_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_31_0_1)
      + CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(ac_math_atan_pi_2mi_return_69_38_sva_3),
      32), 35), 35));
  acc_tmp_vinit_ndx_sva_mx0w1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(WRITE_i_18_0_sva_18_14
      & WRITE_i_18_0_sva_13_11 & WRITE_i_18_0_sva_10_0) + UNSIGNED'( "1111111111111111111"),
      19));
  for_1_t_7_0_sva_mx1 <= MUX_v_8_2_2((HACC_idx_acc_2_itm_1_14_2(7 DOWNTO 0)), for_1_t_7_0_sva_1_1,
      and_dcpl_60);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl <=
      STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED('1' & (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_2(4
      DOWNTO 2))) + SIGNED'( "0001"), 4));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl(3);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_2
      <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1_1)
      + SIGNED'( "00001"), 5));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5_mx0
      <= MUX_v_21_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_25_5,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5,
      or_dcpl_42);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0_mx0
      <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_4_0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0,
      or_dcpl_42);
  for_1_acc_5_psp_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(for_1_acc_4_itm_1(23
      DOWNTO 5)), 19), 21) + UNSIGNED((HACC_idx_18_2_sva_2(6 DOWNTO 0)) & STD_LOGIC_VECTOR'(
      "000000") & (HACC_idx_18_2_sva_1(7 DOWNTO 0))), 21));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_nl <=
      NOT exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1_1
      <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_nl <=
      STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED((NOT for_1_acc_5_psp_1)
      & (NOT (for_1_acc_4_itm_1(4 DOWNTO 0)))), 26), 27) + UNSIGNED'( "001000000000000000000000001"),
      27));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_nl(26);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_3_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs,
      HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1
      <= NOT(MUX_v_3_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1,
      1),3)), STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_3_nl));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_34_32_1
      <= MUX1HOT_v_3_3_2((HACC_mul_1_itm_1(34 DOWNTO 32)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),3)), STD_LOGIC_VECTOR'( "001"), STD_LOGIC_VECTOR'( (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3)
      & HACC_stage_0 & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_31_0_1
      <= (HACC_mul_1_itm_1(31 DOWNTO 0)) AND STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(NOT
      HACC_stage_0, 1),32)) AND STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1,
      1),32));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_ssc_1
      <= HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1
      <= HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_7_itm_3;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1
      <= NOT(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse
      OR HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_50_nl
      <= MUX_v_3_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(32
      DOWNTO 30)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),3)), HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_69_nl
      <= NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_32_30
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_50_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_69_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_29
      <= ((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(29))
      AND (NOT HACC_stage_0_2)) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_47_nl
      <= MUX_v_2_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(28
      DOWNTO 27)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),2)), HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_76_nl
      <= NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_28_27
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_47_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_76_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_57_nl
      <= NOT HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_8_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(26
      DOWNTO 25)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_57_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_26_25
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_8_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_43_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(24)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_24
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_43_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_59_nl
      <= NOT HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_6_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(23
      DOWNTO 22)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_59_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_23_22
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_6_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_39_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(21)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_21
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_39_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_60_nl
      <= NOT HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_4_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(20
      DOWNTO 18)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_60_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_20_18
      <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_4_nl,
      STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_35_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(17)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_17
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_35_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_16
      <= ((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(16))
      AND (NOT HACC_stage_0_2)) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_32_nl
      <= MUX_v_2_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(15
      DOWNTO 14)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),2)), HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_53_nl
      <= NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_15_14
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_32_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_53_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_51_nl
      <= NOT HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_nl
      <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(13
      DOWNTO 11)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_51_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_13_11
      <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_3_nl,
      STD_LOGIC_VECTOR'("111"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_36_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(10)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_10
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_36_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_50_nl
      <= NOT HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(9
      DOWNTO 8)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_50_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_9_8
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_5_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_40_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(7)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_7 <=
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_40_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_58_nl
      <= NOT HACC_stage_0_2;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_nl
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(6
      DOWNTO 5)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_58_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_6_5
      <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_nl,
      STD_LOGIC_VECTOR'("11"), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_44_nl
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(4)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      HACC_stage_0_2);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_4 <=
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_44_nl
      AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_3 <=
      ((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(3))
      AND (NOT HACC_stage_0_2)) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_2_1
      <= MUX1HOT_v_2_3_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(2
      DOWNTO 1)), STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_1_cse,
      1),2)), STD_LOGIC_VECTOR'( "01"), STD_LOGIC_VECTOR'( (NOT HACC_stage_0_6) &
      HACC_stage_0_2 & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_0 <=
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2(0))
      AND (NOT HACC_stage_0_2) AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_1_ssc_1);
  HACC_round_r_HACC_round_r_acc_conv_2f_and_nl <= HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1
      AND (HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1(10));
  HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1 <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1
      & HACC_ac_fixed_cctor_sva_1_15_14) + CONV_SIGNED(CONV_UNSIGNED(HACC_round_r_HACC_round_r_acc_conv_2f_and_nl,
      1), 13), 13));
  HACC_acc_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HACC_mul_itm_1) + UNSIGNED(HACC_mul_1_itm_1),
      36));
  HACC_acc_itm_35_10 <= HACC_acc_nl(35 DOWNTO 10);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_32_29
      <= MUX_v_4_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(32
      DOWNTO 29)), (STD_LOGIC_VECTOR'( "000") & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_68_nl
      <= NOT HACC_stage_0_4;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_28_27
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(28
      DOWNTO 27)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_68_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_26
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(26)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_25
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(25)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_24
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(24))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_23
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(23)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_22
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(22)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_21
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(21))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_20
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(20)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_19
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(19)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_18
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(18)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_17
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(17))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_16
      <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(16)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1,
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_52_nl
      <= NOT HACC_stage_0_4;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_15_14
      <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(15
      DOWNTO 14)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_not_52_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_13_11
      <= MUX_v_3_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(13
      DOWNTO 11)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1,
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_10
      <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(10))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_9 <=
      MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(9)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_8 <=
      MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(8)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_7 <=
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(7))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_6 <=
      MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(6)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_5 <=
      MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(5)),
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_1(2)),
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_4 <=
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(4))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_3 <=
      MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(3)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1,
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_2 <=
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(2))
      AND (NOT HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_1 <=
      MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(1)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_1_cse_1,
      HACC_stage_0_4);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_0 <=
      (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2(0))
      AND (NOT HACC_stage_0_4);
  WRITE_acc_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(z_out_4(18
      DOWNTO 7)), 12), 13) + SIGNED'( "1001111001011"), 13));
  WRITE_acc_itm_12_1 <= WRITE_acc_nl(12);
  HROW_equal_tmp <= CONV_SL_1_1(UNSIGNED(HROW_y_sva) = UNSIGNED(z_out_5(9 DOWNTO
      0)));
  HCOL_equal_tmp <= CONV_SL_1_1(UNSIGNED(WRITE_i_18_0_sva_10_0) = UNSIGNED(z_out_5(10
      DOWNTO 0)));
  operator_8_false_acc_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(z_out_3(7
      DOWNTO 2)), 6), 7) + UNSIGNED'( "1010011"), 7));
  operator_8_false_acc_itm_6_1 <= operator_8_false_acc_nl(6);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl
      <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED((NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_34_32_1)
      & (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_2_31_0_1)),
      35), 36) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_25_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_4_0
      & STD_LOGIC_VECTOR'( "00000000")), 34), 36) + UNSIGNED'( "000000000000000000000000000000000001"),
      36));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl(35);
  for_for_nor_tmp <= NOT(CONV_SL_1_1(WRITE_i_18_0_sva_18_14/=STD_LOGIC_VECTOR'("00000"))
      OR CONV_SL_1_1(WRITE_i_18_0_sva_13_11/=STD_LOGIC_VECTOR'("000")) OR CONV_SL_1_1(WRITE_i_18_0_sva_10_0/=STD_LOGIC_VECTOR'("00000000000")));
  and_dcpl <= NOT(for_1_stage_0_2 OR for_1_stage_0);
  and_36_cse <= (NOT HROW_equal_tmp) AND (fsm_output(9));
  and_dcpl_25 <= NOT(for_1_stage_0_3 OR for_1_stage_0_2);
  and_dcpl_26 <= and_dcpl_25 AND (NOT for_1_stage_0_4) AND (NOT for_1_stage_0);
  and_dcpl_33 <= NOT(HACC_stage_0 OR HACC_stage_0_2);
  and_dcpl_35 <= for_1_stage_0_4 AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_3);
  or_dcpl_36 <= HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 OR (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3);
  and_dcpl_43 <= (NOT for_1_stage_0_2) AND for_1_stage_0_4;
  or_dcpl_38 <= NOT((NOT(and_dcpl_25 AND (NOT for_1_stage_0))) AND for_1_asn_7_itm_3);
  or_dcpl_42 <= (NOT for_1_stage_0_4) OR for_1_asn_7_itm_3;
  and_dcpl_53 <= for_1_stage_0_4 AND (NOT for_1_asn_7_itm_3);
  and_dcpl_60 <= for_1_stage_0_2 AND (NOT for_1_asn_7_itm_1) AND (NOT reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_1_cse);
  and_87_cse <= HROW_equal_tmp AND (fsm_output(9));
  and_88_cse <= CONV_SL_1_1(fsm_output(2 DOWNTO 1)/=STD_LOGIC_VECTOR'("00"));
  or_tmp_59 <= NOT((fsm_output(11)) OR (fsm_output(0)) OR (fsm_output(10)) OR (fsm_output(1)));
  or_tmp_81 <= (fsm_output(5)) OR (fsm_output(9));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_rmff <=
      NOT(and_dcpl_35 AND (fsm_output(3)));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff
      <= (NOT(CONV_SL_1_1(fsm_output(6 DOWNTO 5)/=STD_LOGIC_VECTOR'("00")))) OR ((NOT
      (z_out_4(8))) AND (fsm_output(5))) OR ((NOT(HACC_stage_0 AND operator_8_false_acc_itm_6_1))
      AND (fsm_output(6)));
  WRITE_i_WRITE_i_and_nl <= MUX_v_19_2_2(STD_LOGIC_VECTOR'("0000000000000000000"),
      z_out_4, (fsm_output(10)));
  for_and_nl <= (NOT (fsm_output(2))) AND and_88_cse;
  for_or_1_nl <= CONV_SL_1_1(fsm_output(10 DOWNTO 9)/=STD_LOGIC_VECTOR'("00"));
  for_mux1h_6_nl <= MUX1HOT_v_19_4_2(STD_LOGIC_VECTOR'( "1100001101001111111"), acc_tmp_vinit_ndx_sva_mx0w1,
      (HACC_idx_18_2_sva_2 & HACC_idx_slc_HACC_t_1_0_itm_6), WRITE_i_WRITE_i_and_nl,
      STD_LOGIC_VECTOR'( for_and_nl & (fsm_output(2)) & (fsm_output(6)) & for_or_1_nl));
  acc_tmp_rsci_addr_d <= z_out_2 & HACC_idx_slc_HACC_t_1_0_1_itm_4 & for_mux1h_6_nl;
  or_95_nl <= and_87_cse OR (HACC_stage_0 AND WRITE_acc_itm_12_1 AND (fsm_output(10)));
  for_for_mux_1_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'( "01"), STD_LOGIC_VECTOR'( "10"),
      or_95_nl);
  or_93_nl <= (NOT((fsm_output(10)) OR (fsm_output(6)) OR (fsm_output(9)))) OR and_36_cse
      OR ((NOT HACC_stage_0_5) AND (fsm_output(6))) OR ((NOT(HACC_stage_0 AND WRITE_acc_itm_12_1))
      AND (fsm_output(10)));
  acc_tmp_rsci_re_d <= MUX_v_2_2_2(for_for_mux_1_nl, STD_LOGIC_VECTOR'("11"), or_93_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_2_nl
      <= (NOT((fsm_output(2)) OR (fsm_output(1)) OR (fsm_output(6)))) OR ((NOT HACC_stage_0_7)
      AND (fsm_output(6))) OR (for_for_nor_tmp AND (fsm_output(2)));
  acc_tmp_rsci_we_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_2_nl);
  cos_out_rsci_re_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff);
  cos_out_rsci_we_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_rmff);
  sin_out_rsci_re_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff);
  sin_out_rsci_we_d <= STD_LOGIC_VECTOR'( '1' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_rmff);
  acc_tmp_rsci_data_in_d <= MUX_v_16_2_2(STD_LOGIC_VECTOR'("0000000000000000"), z_out_5,
      (fsm_output(6)));
  cos_out_rsci_data_in_d <= z_out(32 DOWNTO 6);
  HACC_t_HACC_t_and_nl <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), z_out_3, (fsm_output(6)));
  or_216_nl <= CONV_SL_1_1(fsm_output(6 DOWNTO 5)/=STD_LOGIC_VECTOR'("00"));
  cos_out_rsci_addr_d <= MUX_v_8_2_2((HACC_acc_4_itm_1(7 DOWNTO 0)), HACC_t_HACC_t_and_nl,
      or_216_nl);
  sin_out_rsci_data_in_d <= z_out_1(32 DOWNTO 6);
  or_tmp_123 <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1)
      AND (fsm_output(3));
  for_1_or_3_ssc <= (fsm_output(4)) OR (fsm_output(10));
  for_1_or_6_ssc <= (fsm_output(8)) OR (fsm_output(6));
  for_1_nor_1_seb <= NOT(CONV_SL_1_1(fsm_output(6 DOWNTO 3)/=STD_LOGIC_VECTOR'("0000")));
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= '0';
        reg_sin_out_rsc_cgo_cse <= '0';
        reg_acc_tmp_rsc_cgo_cse <= '0';
        reg_acc_rsci_ivld_core_psct_cse <= '0';
        reg_data_in_rsci_irdy_core_psct_cse <= '0';
        operator_10_false_io_read_heightIn_rsc_cse_sva <= STD_LOGIC_VECTOR'( "0000000000");
        operator_11_false_io_read_widthIn_rsc_cse_sva <= STD_LOGIC_VECTOR'( "00000000000");
        WRITE_i_18_0_sva_18_14 <= STD_LOGIC_VECTOR'( "00000");
        WRITE_i_18_0_sva_13_11 <= STD_LOGIC_VECTOR'( "000");
        WRITE_i_18_0_sva_10_0 <= STD_LOGIC_VECTOR'( "00000000000");
        HACC_mul_1_itm_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2
            <= '0';
        for_1_t_7_0_sva_1_1 <= STD_LOGIC_VECTOR'( "00000000");
        for_1_stage_0 <= '0';
        exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
            <= '0';
        for_1_stage_0_2 <= '0';
        for_1_stage_0_3 <= '0';
        for_1_stage_0_4 <= '0';
        for_1_stage_0_5 <= '0';
        HACC_stage_0 <= '0';
        HACC_stage_0_2 <= '0';
        HACC_stage_0_3 <= '0';
        HACC_stage_0_4 <= '0';
        HACC_stage_0_5 <= '0';
        HACC_stage_0_6 <= '0';
        HACC_stage_0_7 <= '0';
        HACC_stage_0_8 <= '0';
        HACC_ac_fixed_cctor_sva_1_15_14 <= STD_LOGIC_VECTOR'( "00");
        HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 <= '0';
        HACC_t_sva <= STD_LOGIC_VECTOR'( "00000000");
        HACC_acc_4_itm_1 <= STD_LOGIC_VECTOR'( "000000000000");
        HACC_acc_5_itm_1 <= STD_LOGIC_VECTOR'( "00000000000");
        HACC_idx_18_2_sva_1 <= STD_LOGIC_VECTOR'( "00000000000000000");
        HACC_idx_18_2_sva_2 <= STD_LOGIC_VECTOR'( "00000000000000000");
        HACC_idx_acc_2_itm_1_14_2 <= STD_LOGIC_VECTOR'( "0000000000000");
        HACC_mul_itm_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000000");
      ELSIF ( core_wen = '1' ) THEN
        reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= and_dcpl_33 AND (fsm_output(10));
        reg_sin_out_rsc_cgo_cse <= or_71_rmff;
        reg_acc_tmp_rsc_cgo_cse <= or_72_rmff;
        reg_acc_rsci_ivld_core_psct_cse <= HACC_stage_0_2 AND (fsm_output(10));
        reg_data_in_rsci_irdy_core_psct_cse <= ((NOT HCOL_equal_tmp) AND (fsm_output(7)))
            OR (and_dcpl_26 AND (fsm_output(3))) OR and_36_cse;
        operator_10_false_io_read_heightIn_rsc_cse_sva <= MUX_v_10_2_2(heightIn_rsci_idat,
            operator_10_false_io_read_heightIn_rsc_cse_sva, or_tmp_59);
        operator_11_false_io_read_widthIn_rsc_cse_sva <= MUX_v_11_2_2(widthIn_rsci_idat,
            operator_11_false_io_read_widthIn_rsc_cse_sva, or_tmp_59);
        WRITE_i_18_0_sva_18_14 <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), WRITE_i_mux1h_4_nl,
            nor_16_seb);
        WRITE_i_18_0_sva_13_11 <= MUX_v_3_2_2(WRITE_i_asn_WRITE_i_18_0_sva_WRITE_i_and_1_nl,
            (z_out_4(13 DOWNTO 11)), or_211_nl);
        WRITE_i_18_0_sva_10_0 <= MUX_v_11_2_2(STD_LOGIC_VECTOR'("00000000000"), WRITE_i_mux1h_9_nl,
            and_nl);
        HACC_mul_1_itm_1 <= MUX_v_36_2_2(('0' & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux1h_2_nl),
            STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_mul_1_nl), 36)), fsm_output(6));
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1;
        for_1_t_7_0_sva_1_1 <= MUX_v_8_2_2(z_out_3, (STD_LOGIC_VECTOR'( "00") & (HACC_t_sva(7
            DOWNTO 2))), fsm_output(6));
        for_1_stage_0 <= NOT((NOT(for_1_stage_0 AND (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1
            OR (z_out_2(6))))) AND (fsm_output(3)));
        exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva
            <= NOT(((NOT (z_out_2(6))) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1)
            AND (fsm_output(3)));
        for_1_stage_0_2 <= WRITE_i_and_1_cse;
        for_1_stage_0_3 <= for_1_stage_0_2 AND (fsm_output(3));
        for_1_stage_0_4 <= for_1_stage_0_3 AND (fsm_output(3));
        for_1_stage_0_5 <= for_1_stage_0_4 AND (fsm_output(3));
        HACC_stage_0 <= HACC_mux1h_5_nl OR or_tmp_81;
        HACC_stage_0_2 <= HACC_mux_6_nl AND (NOT or_tmp_81);
        HACC_stage_0_3 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_59_nl
            AND (NOT (fsm_output(5)));
        HACC_stage_0_4 <= HACC_stage_0_3 AND (NOT (fsm_output(5)));
        HACC_stage_0_5 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_58_nl
            AND (NOT (fsm_output(5)));
        HACC_stage_0_6 <= HACC_stage_0_5 AND (NOT (fsm_output(5)));
        HACC_stage_0_7 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_57_nl
            AND (NOT (fsm_output(5)));
        HACC_stage_0_8 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_56_nl
            AND (NOT (fsm_output(5)));
        HACC_ac_fixed_cctor_sva_1_15_14 <= MUX_v_2_2_2((for_1_acc_5_psp_1(20 DOWNTO
            19)), (HACC_acc_itm_35_10(15 DOWNTO 14)), fsm_output(6));
        HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1,
            HACC_round_r_HACC_round_r_acc_conv_2f_or_nl, fsm_output(6));
        HACC_t_sva <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), HACC_t_mux1h_3_nl,
            Hough_Algorithm_HW_1296_864_houghTransform_not_nl);
        HACC_acc_4_itm_1 <= MUX1HOT_v_12_3_2((STD_LOGIC_VECTOR'( "0000") & HACC_t_sva),
            (z_out_4(11 DOWNTO 0)), (STD_LOGIC_VECTOR'( "00") & (z_out_5(9 DOWNTO
            0))), STD_LOGIC_VECTOR'( (fsm_output(3)) & (fsm_output(6)) & (fsm_output(8))));
        HACC_acc_5_itm_1 <= MUX_v_11_2_2((STD_LOGIC_VECTOR'( "0000") & (NOT (for_1_t_7_0_sva_mx1(6
            DOWNTO 0)))), STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_acc_5_nl), 11)),
            fsm_output(6));
        HACC_idx_18_2_sva_1 <= MUX_v_17_2_2((STD_LOGIC_VECTOR'( "000000000") & (HACC_idx_acc_2_itm_1_14_2(7
            DOWNTO 0))), z_out_2, fsm_output(6));
        HACC_idx_18_2_sva_2 <= MUX_v_17_2_2((STD_LOGIC_VECTOR'( "0000000000") & (HACC_idx_acc_2_itm_1_14_2(6
            DOWNTO 0))), HACC_idx_18_2_sva_1, fsm_output(6));
        HACC_idx_acc_2_itm_1_14_2 <= MUX_v_13_2_2((STD_LOGIC_VECTOR'( "00000") &
            for_1_t_for_1_t_and_nl), STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HACC_idx_acc_6_nl),
            13)), fsm_output(6));
        HACC_mul_itm_1 <= MUX_v_36_2_2((STD_LOGIC_VECTOR'( "0000") & ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_itm),
            STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_mul_nl), 36)), fsm_output(6));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        acc_rsci_idat <= STD_LOGIC_VECTOR'( "0000000000000000");
      ELSIF ( (core_wen AND (fsm_output(10)) AND HACC_stage_0_2) = '1' ) THEN
        acc_rsci_idat <= acc_tmp_rsci_data_out_d_oreg(15 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
            <= '0';
      ELSIF ( (core_wen AND (NOT((NOT (fsm_output(3))) OR or_dcpl_36 OR (NOT for_1_stage_0_4))))
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx0w0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3
            <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000000");
      ELSIF ( (core_wen AND (fsm_output(3)) AND mux_6_nl AND (for_1_stage_0_2 OR
          and_60_rgt OR and_61_rgt)) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3
            <= MUX1HOT_v_35_3_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w0,
            ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w1,
            (STD_LOGIC_VECTOR'( "000") & (HACC_mul_itm_1(31 DOWNTO 0))), STD_LOGIC_VECTOR'(
            and_60_rgt & and_61_rgt & for_1_stage_0_2));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2
            <= STD_LOGIC_VECTOR'( "000000000000000000000000000000000");
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2
            <= z_out;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2
            <= z_out_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_4
            <= '0';
      ELSIF ( (core_wen AND for_1_stage_0_4) = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_4
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_3;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_1_asn_7_itm_3 <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_3
            <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_25_5
            <= STD_LOGIC_VECTOR'( "000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_4_0
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3
            <= '0';
      ELSIF ( for_1_and_cse = '1' ) THEN
        for_1_asn_7_itm_3 <= for_1_asn_7_itm_2;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_3
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_2;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_25_5
            <= MUX_v_21_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5_mx0,
            for_1_acc_5_psp_1, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2);
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_dfm_1_4_0
            <= MUX_v_5_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0_mx0,
            (for_1_acc_4_itm_1(4 DOWNTO 0)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2);
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_9_itm_3
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_1_acc_4_itm_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000");
      ELSIF ( (core_wen AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
          AND for_1_stage_0_2) = '1' ) THEN
        for_1_acc_4_itm_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED((HACC_idx_acc_2_itm_1_14_2(7
            DOWNTO 0)) & STD_LOGIC_VECTOR'( "000000") & (HACC_idx_acc_2_itm_1_14_2(7
            DOWNTO 0)) & STD_LOGIC_VECTOR'( "01")) + CONV_UNSIGNED(CONV_SIGNED(SIGNED('1'
            & (z_out_5(13 DOWNTO 0)) & (HACC_acc_5_itm_1(6 DOWNTO 0))), 22), 24),
            24));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_1_cse
            <= '0';
        for_1_asn_7_itm_1 <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
            <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_7_cse
          = '1' ) THEN
        reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_1_cse
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1;
        for_1_asn_7_itm_1 <= NOT((z_out_2(6)) OR ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1);
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1
            <= exitL_exit_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_sva;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( (core_wen AND for_1_stage_0 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_itm_3_1)
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_2;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5
            <= STD_LOGIC_VECTOR'( "000000000000000000000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( and_355_cse = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_25_5_mx0;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_lpi_2_4_0_mx0;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_atan_pi_2mi_return_69_38_sva_3 <= STD_LOGIC_VECTOR'( "00000000000000000000000000000000");
      ELSIF ( (core_wen AND (and_dcpl OR (NOT for_1_asn_7_itm_2)) AND for_1_stage_0_3)
          = '1' ) THEN
        ac_math_atan_pi_2mi_return_69_38_sva_3 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3(31
            DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_3
            <= STD_LOGIC_VECTOR'( "00000");
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_7_itm_3
            <= '0';
      ELSIF ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_and_1_cse
          = '1' ) THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_3
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_2;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_7_itm_3
            <= HACC_stage_0_8;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        for_1_asn_7_itm_2 <= '0';
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_2
            <= '0';
      ELSIF ( for_1_and_7_cse = '1' ) THEN
        for_1_asn_7_itm_2 <= for_1_asn_7_itm_1;
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_st_2
            <= reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_1_cse;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_2
            <= STD_LOGIC_VECTOR'( "00000");
      ELSIF ( (core_wen AND (NOT(reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_svs_1_cse
          AND for_1_asn_7_itm_1 AND for_1_stage_0)) AND for_1_stage_0_2) = '1' )
          THEN
        ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_2
            <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_lpi_2_dfm_1;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HROW_y_sva <= STD_LOGIC_VECTOR'( "0000000000");
      ELSIF ( (core_wen AND ((fsm_output(3)) OR (fsm_output(9)))) = '1' ) THEN
        HROW_y_sva <= MUX_v_10_2_2(STD_LOGIC_VECTOR'("0000000000"), (HACC_acc_4_itm_1(9
            DOWNTO 0)), (fsm_output(9)));
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HCOL_acc_1_itm <= STD_LOGIC_VECTOR'( "00000000000");
      ELSIF ( (core_wen AND (fsm_output(4))) = '1' ) THEN
        HCOL_acc_1_itm <= z_out_4(10 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_idx_slc_HACC_t_1_0_itm_6 <= STD_LOGIC_VECTOR'( "00");
      ELSIF ( (core_wen AND HACC_stage_0_6) = '1' ) THEN
        HACC_idx_slc_HACC_t_1_0_itm_6 <= HACC_idx_slc_HACC_t_1_0_itm_5;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_idx_slc_HACC_t_1_0_1_itm_4 <= STD_LOGIC_VECTOR'( "00");
        HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1 <= STD_LOGIC_VECTOR'(
            "0000000000000");
        HACC_idx_slc_HACC_t_7_2_itm_4 <= STD_LOGIC_VECTOR'( "000000");
      ELSIF ( HACC_idx_and_1_cse = '1' ) THEN
        HACC_idx_slc_HACC_t_1_0_1_itm_4 <= HACC_idx_slc_HACC_t_1_0_1_itm_3;
        HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1 <= HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1;
        HACC_idx_slc_HACC_t_7_2_itm_4 <= HACC_idx_slc_HACC_t_7_2_itm_3;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1 <= STD_LOGIC_VECTOR'(
            "00000000000");
        HACC_idx_slc_HACC_t_1_0_1_itm_3 <= STD_LOGIC_VECTOR'( "00");
        HACC_idx_slc_HACC_t_7_2_itm_3 <= STD_LOGIC_VECTOR'( "000000");
      ELSIF ( HACC_round_r_HACC_round_r_acc_conv_2f_and_1_cse = '1' ) THEN
        HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED(HACC_acc_itm_35_10(25
            DOWNTO 16)), 10), 11) + UNSIGNED'( "00011100101"), 11));
        HACC_idx_slc_HACC_t_1_0_1_itm_3 <= HACC_idx_slc_HACC_t_1_0_1_itm_2;
        HACC_idx_slc_HACC_t_7_2_itm_3 <= HACC_idx_slc_HACC_t_7_2_itm_2;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_idx_slc_HACC_t_1_0_itm_5 <= STD_LOGIC_VECTOR'( "00");
      ELSIF ( (core_wen AND HACC_stage_0_5) = '1' ) THEN
        HACC_idx_slc_HACC_t_1_0_itm_5 <= HACC_idx_slc_HACC_t_1_0_1_itm_4;
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_idx_slc_HACC_t_1_0_1_itm_2 <= STD_LOGIC_VECTOR'( "00");
        HACC_idx_slc_HACC_t_7_2_itm_2 <= STD_LOGIC_VECTOR'( "000000");
      ELSIF ( HACC_idx_and_8_cse = '1' ) THEN
        HACC_idx_slc_HACC_t_1_0_1_itm_2 <= HACC_idx_slc_HACC_t_1_0_1_itm_1;
        HACC_idx_slc_HACC_t_7_2_itm_2 <= for_1_t_7_0_sva_1_1(5 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  PROCESS (clk)
  BEGIN
    IF clk'EVENT AND ( clk = '1' ) THEN
      IF (rst = '1') THEN
        HACC_idx_slc_HACC_t_1_0_1_itm_1 <= STD_LOGIC_VECTOR'( "00");
      ELSIF ( (core_wen AND HACC_stage_0) = '1' ) THEN
        HACC_idx_slc_HACC_t_1_0_1_itm_1 <= HACC_t_sva(1 DOWNTO 0);
      END IF;
    END IF;
  END PROCESS;
  WRITE_i_mux1h_4_nl <= MUX1HOT_v_5_3_2(STD_LOGIC_VECTOR'( "11000"), (acc_tmp_vinit_ndx_sva_mx0w1(18
      DOWNTO 14)), (z_out_4(18 DOWNTO 14)), STD_LOGIC_VECTOR'( (fsm_output(1)) &
      (fsm_output(2)) & (fsm_output(10))));
  WRITE_i_WRITE_i_mux_nl <= MUX_v_3_2_2(STD_LOGIC_VECTOR'( "011"), (acc_tmp_vinit_ndx_sva_mx0w1(13
      DOWNTO 11)), fsm_output(2));
  WRITE_i_asn_WRITE_i_18_0_sva_WRITE_i_and_1_nl <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"),
      WRITE_i_WRITE_i_mux_nl, nor_16_seb);
  or_211_nl <= (fsm_output(3)) OR (fsm_output(10));
  WRITE_i_WRITE_i_WRITE_i_or_nl <= ((NOT for_1_stage_0) AND (fsm_output(3))) OR CONV_SL_1_1(fsm_output(6
      DOWNTO 4)/=STD_LOGIC_VECTOR'("000")) OR and_36_cse;
  WRITE_i_or_1_nl <= WRITE_i_and_1_cse OR (fsm_output(10));
  WRITE_i_mux1h_9_nl <= MUX1HOT_v_11_5_2(STD_LOGIC_VECTOR'( "01001111111"), (acc_tmp_vinit_ndx_sva_mx0w1(10
      DOWNTO 0)), WRITE_i_18_0_sva_10_0, HCOL_acc_1_itm, (z_out_4(10 DOWNTO 0)),
      STD_LOGIC_VECTOR'( (fsm_output(1)) & (fsm_output(2)) & WRITE_i_WRITE_i_WRITE_i_or_nl
      & (fsm_output(7)) & WRITE_i_or_1_nl));
  and_nl <= nor_16_seb AND (NOT and_36_cse) AND (NOT((NOT for_1_stage_0) AND (fsm_output(3))));
  or_114_nl <= (or_dcpl_42 AND (fsm_output(3))) OR (fsm_output(2));
  and_185_nl <= and_dcpl_53 AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      AND (fsm_output(3));
  and_187_nl <= and_dcpl_53 AND (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1)
      AND (fsm_output(3));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux1h_2_nl
      <= MUX1HOT_v_35_4_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3,
      (HACC_mul_1_itm_1(34 DOWNTO 0)), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_3_mx1w1,
      STD_LOGIC_VECTOR'( (fsm_output(1)) & or_114_nl & and_185_nl & and_187_nl));
  HACC_mul_1_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED'( SIGNED(sin_out_rsci_data_out_d_oreg)
      * SIGNED(HACC_acc_5_itm_1)), 36));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_2_nl
      <= (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1)
      AND ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_2;
  T_LINE_if_aelse_T_LINE_if_aelse_and_1_nl <= HACC_stage_0 AND operator_8_false_acc_itm_6_1;
  and_23_nl <= HACC_stage_0 AND WRITE_acc_itm_12_1;
  HACC_mux1h_5_nl <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_2_nl,
      T_LINE_if_aelse_T_LINE_if_aelse_and_1_nl, and_23_nl, STD_LOGIC_VECTOR'( (fsm_output(3))
      & (fsm_output(6)) & (fsm_output(10))));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_nl <=
      (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_acc_itm_26_1)
      AND HACC_stage_0_7;
  or_144_nl <= (fsm_output(10)) OR (fsm_output(6));
  HACC_mux_6_nl <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_nl,
      HACC_stage_0, or_144_nl);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_59_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1,
      HACC_stage_0_2, fsm_output(6));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_58_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1,
      HACC_stage_0_4, fsm_output(6));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_57_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1,
      HACC_stage_0_6, fsm_output(6));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_56_nl
      <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_asn_itm_1,
      HACC_stage_0_7, fsm_output(6));
  HACC_round_r_HACC_round_r_acc_conv_2f_or_nl <= CONV_SL_1_1(HACC_acc_itm_35_10(13
      DOWNTO 0)/=STD_LOGIC_VECTOR'("00000000000000"));
  HACC_t_mux1h_3_nl <= MUX1HOT_v_8_3_2((HACC_idx_acc_2_itm_1_14_2(7 DOWNTO 0)), data_in_rsci_idat_mxwt,
      z_out_3, STD_LOGIC_VECTOR'( (fsm_output(3)) & (fsm_output(4)) & (fsm_output(6))));
  Hough_Algorithm_HW_1296_864_houghTransform_not_nl <= NOT (fsm_output(5));
  HACC_acc_5_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(CONV_SIGNED(CONV_UNSIGNED(UNSIGNED(HROW_y_sva),
      10), 11) + CONV_SIGNED(CONV_SIGNED(SIGNED('1' & (NOT (operator_10_false_io_read_heightIn_rsc_cse_sva(9
      DOWNTO 1)))), 10), 11) + SIGNED'( "00000000001"), 11));
  and_217_nl <= and_dcpl_60 AND (fsm_output(3));
  for_1_t_mux_2_nl <= MUX_v_8_2_2((HACC_idx_acc_2_itm_1_14_2(7 DOWNTO 0)), for_1_t_7_0_sva_1_1,
      and_217_nl);
  for_1_t_for_1_t_and_nl <= MUX_v_8_2_2(STD_LOGIC_VECTOR'("00000000"), for_1_t_mux_2_nl,
      (fsm_output(3)));
  HACC_idx_acc_8_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1(12
      DOWNTO 4)) + SIGNED'( "000000001"), 9));
  HACC_idx_acc_6_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(CONV_UNSIGNED(CONV_SIGNED(SIGNED(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_idx_acc_8_nl),
      9)) & (HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1(3 DOWNTO 2))),
      11), 13) + UNSIGNED(NOT HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1),
      13));
  HACC_mul_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED'( SIGNED(cos_out_rsci_data_out_d_oreg)
      * SIGNED(HACC_acc_4_itm_1)), 36));
  or_181_nl <= for_1_stage_0_2 OR (NOT for_1_stage_0_3);
  nor_5_nl <= NOT(for_1_asn_7_itm_1 OR (NOT for_1_stage_0_2));
  mux_6_nl <= MUX_s_1_2_2(or_181_nl, nor_5_nl, for_1_stage_0);
  nand_13_cse <= NOT(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      AND (fsm_output(3)));
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_2_nl
      <= nand_13_cse OR or_tmp_123;
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_48_nl
      <= MUX_v_33_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1,
      (NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1),
      or_tmp_123);
  acc_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_32_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_26
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_23
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_20
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_19
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_9
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_6
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_2
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_conc_33_0
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_2_nl)
      + UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_48_nl
      & '1'), 34));
  z_out <= acc_nl(33 DOWNTO 1);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_nand_1_nl
      <= NOT(or_tmp_123 AND nand_13_cse);
  ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_49_nl
      <= MUX_v_33_2_2((NOT ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1,
      or_tmp_123);
  acc_1_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_32_30
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_29
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_28_27
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_26_25
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_24
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_23_22
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_21
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_20_18
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_17
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_16
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_15_14
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_13_11
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_10
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_9_8
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_7
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_6_5
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_4
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_3
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_2_1
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_25_0
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_nand_1_nl)
      + UNSIGNED(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_49_nl
      & '1'), 34));
  z_out_1 <= acc_1_nl(33 DOWNTO 1);
  HACC_idx_acc_9_nl <= STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED((HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1(10
      DOWNTO 0)) & HACC_idx_slc_HACC_t_7_2_itm_4) + CONV_SIGNED(CONV_SIGNED(SIGNED(HACC_idx_acc_2_itm_1_14_2
      & (HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1(1 DOWNTO 0))), 15),
      17), 17));
  HACC_idx_mux_5_nl <= MUX_v_17_2_2(STD_LOGIC_VECTOR(CONV_SIGNED(SIGNED(HACC_idx_acc_9_nl),
      17)), (STD_LOGIC_VECTOR'( "00000000000") & (z_out_3(7 DOWNTO 2))), fsm_output(3));
  HACC_idx_mux_6_nl <= MUX_v_13_2_2((NOT HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1),
      STD_LOGIC_VECTOR'( "1111111111101"), fsm_output(3));
  z_out_2 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(HACC_idx_mux_5_nl) + UNSIGNED(HACC_idx_mux_6_nl
      & '0' & (NOT (fsm_output(3))) & (fsm_output(3)) & (fsm_output(3))), 17));
  for_1_mux_11_nl <= MUX_v_8_2_2(for_1_t_7_0_sva_mx1, HACC_t_sva, fsm_output(6));
  z_out_3 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(for_1_mux_11_nl) + UNSIGNED'(
      "00000001"), 8));
  for_1_for_1_and_9_nl <= MUX_v_5_2_2(STD_LOGIC_VECTOR'("00000"), WRITE_i_18_0_sva_18_14,
      for_1_nor_1_seb);
  for_1_for_1_and_10_nl <= (WRITE_i_18_0_sva_13_11(2)) AND for_1_nor_1_seb;
  for_1_mux_12_nl <= MUX_v_2_2_2((NOT (for_1_t_7_0_sva_mx1(7 DOWNTO 6))), (WRITE_i_18_0_sva_13_11(1
      DOWNTO 0)), fsm_output(10));
  for_1_nor_6_nl <= NOT(CONV_SL_1_1(fsm_output(6 DOWNTO 4)/=STD_LOGIC_VECTOR'("000")));
  for_1_for_1_and_11_nl <= MUX_v_2_2_2(STD_LOGIC_VECTOR'("00"), for_1_mux_12_nl,
      for_1_nor_6_nl);
  for_1_or_8_nl <= (fsm_output(6)) OR (fsm_output(4)) OR (fsm_output(10));
  for_1_mux1h_33_nl <= MUX1HOT_v_11_3_2(((NOT (for_1_t_7_0_sva_mx1(5 DOWNTO 0)))
      & STD_LOGIC_VECTOR'( "1111") & (NOT (for_1_t_7_0_sva_mx1(7)))), WRITE_i_18_0_sva_10_0,
      (STD_LOGIC_VECTOR'( "001") & (NOT HACC_t_sva)), STD_LOGIC_VECTOR'( (fsm_output(3))
      & for_1_or_8_nl & (fsm_output(5))));
  for_1_or_9_nl <= (NOT((fsm_output(3)) OR (fsm_output(4)) OR (fsm_output(10)) OR
      (fsm_output(5)))) OR (fsm_output(6));
  for_1_for_1_or_1_nl <= (NOT((fsm_output(3)) OR for_1_or_3_ssc OR (fsm_output(5))))
      OR (fsm_output(6));
  for_1_or_10_nl <= (fsm_output(3)) OR for_1_or_3_ssc OR (fsm_output(5));
  for_1_for_1_for_1_nor_1_nl <= NOT(MUX_v_2_2_2((operator_11_false_io_read_widthIn_rsc_cse_sva(10
      DOWNTO 9)), STD_LOGIC_VECTOR'("11"), for_1_or_10_nl));
  for_1_mux1h_34_nl <= MUX1HOT_v_8_4_2((NOT for_1_t_7_0_sva_mx1), (NOT (operator_11_false_io_read_widthIn_rsc_cse_sva(8
      DOWNTO 1))), STD_LOGIC_VECTOR'( "00000001"), STD_LOGIC_VECTOR'( "11111011"),
      STD_LOGIC_VECTOR'( (fsm_output(3)) & (fsm_output(6)) & for_1_or_3_ssc & (fsm_output(5))));
  acc_4_nl <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(for_1_for_1_and_9_nl & for_1_for_1_and_10_nl
      & for_1_for_1_and_11_nl & for_1_mux1h_33_nl & for_1_or_9_nl) + CONV_UNSIGNED(CONV_SIGNED(SIGNED(for_1_for_1_or_1_nl
      & for_1_for_1_for_1_nor_1_nl & for_1_mux1h_34_nl & '1'), 12), 20), 20));
  z_out_4 <= acc_4_nl(19 DOWNTO 1);
  for_1_mux1h_35_nl <= MUX1HOT_v_16_3_2(STD_LOGIC_VECTOR'( "1101111100000001"), (STD_LOGIC_VECTOR'(
      "000000") & HROW_y_sva), (acc_tmp_rsci_data_out_d_oreg(31 DOWNTO 16)), STD_LOGIC_VECTOR'(
      (fsm_output(3)) & (fsm_output(8)) & (fsm_output(6))));
  for_1_or_12_nl <= (fsm_output(9)) OR (fsm_output(7));
  for_1_or_11_nl <= MUX_v_16_2_2(for_1_mux1h_35_nl, STD_LOGIC_VECTOR'("1111111111111111"),
      for_1_or_12_nl);
  for_1_nor_9_nl <= NOT(for_1_or_6_ssc OR (fsm_output(9)) OR (fsm_output(7)));
  for_1_for_1_and_12_nl <= MUX_v_3_2_2(STD_LOGIC_VECTOR'("000"), WRITE_i_18_0_sva_13_11,
      for_1_nor_9_nl);
  for_1_mux1h_36_nl <= MUX1HOT_v_11_4_2(WRITE_i_18_0_sva_10_0, STD_LOGIC_VECTOR'(
      "00000000001"), ('0' & operator_10_false_io_read_heightIn_rsc_cse_sva), operator_11_false_io_read_widthIn_rsc_cse_sva,
      STD_LOGIC_VECTOR'( (fsm_output(3)) & for_1_or_6_ssc & (fsm_output(9)) & (fsm_output(7))));
  z_out_5 <= STD_LOGIC_VECTOR(CONV_UNSIGNED(UNSIGNED(for_1_or_11_nl) + CONV_UNSIGNED(CONV_UNSIGNED(UNSIGNED(for_1_for_1_and_12_nl
      & for_1_mux1h_36_nl), 14), 16), 16));
END v25;

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

ARCHITECTURE v25 OF getMaxLine IS
  -- Default Constants
  CONSTANT PWR : STD_LOGIC := '1';

  -- Interconnect Declarations
  SIGNAL cos_out_1_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL cos_out_1_rsci_addr_d : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL cos_out_1_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsci_data_out_d : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_1_rsci_en_d : STD_LOGIC;
  SIGNAL sin_out_1_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL sin_out_1_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsci_data_out_d : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_a : STD_LOGIC_VECTOR (17 DOWNTO 0);
  SIGNAL T_LINE_if_if_dividend1_mul_cmp_b : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL cos_out_1_rsc_en : STD_LOGIC;
  SIGNAL cos_out_1_rsc_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_1_rsc_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsc_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsc_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_1_rsc_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_1_rsc_en : STD_LOGIC;
  SIGNAL sin_out_1_rsc_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_1_rsc_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsc_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsc_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_1_rsc_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);

  SIGNAL cos_out_1_rsc_comp_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_1_rsc_comp_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_1_rsc_comp_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsc_comp_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsc_comp_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);

  SIGNAL sin_out_1_rsc_comp_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_1_rsc_comp_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_1_rsc_comp_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsc_comp_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsc_comp_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);

  COMPONENT getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT(
      en : OUT STD_LOGIC;
      data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      en_d : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL cos_out_1_rsci_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_1_rsci_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsci_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsci_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_1_rsci_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_1_rsci_data_in_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_1_rsci_addr_d_1 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_1_rsci_re_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsci_we_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_1_rsci_data_out_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);

  COMPONENT getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT(
      en : OUT STD_LOGIC;
      data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      en_d : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL sin_out_1_rsci_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_1_rsci_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsci_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsci_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_1_rsci_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_1_rsci_data_in_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_1_rsci_addr_d : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_1_rsci_re_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsci_we_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_1_rsci_data_out_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);

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
      cos_out_1_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      cos_out_1_rsci_addr_d : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
      cos_out_1_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      cos_out_1_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      cos_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      cos_out_1_rsci_en_d : OUT STD_LOGIC;
      sin_out_1_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      sin_out_1_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      sin_out_1_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      sin_out_1_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_a : OUT STD_LOGIC_VECTOR (17 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_b : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      T_LINE_if_if_dividend1_mul_cmp_z : IN STD_LOGIC_VECTOR (43 DOWNTO 0)
    );
  END COMPONENT;
  SIGNAL getMaxLine_core_inst_x1_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_y1_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_x2_rsc_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_y2_rsc_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_acc_rsc_dat : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_cos_out_1_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL getMaxLine_core_inst_cos_out_1_rsci_addr_d : STD_LOGIC_VECTOR (7 DOWNTO
      0);
  SIGNAL getMaxLine_core_inst_cos_out_1_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_cos_out_1_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_cos_out_1_rsci_data_out_d : STD_LOGIC_VECTOR (53 DOWNTO
      0);
  SIGNAL getMaxLine_core_inst_sin_out_1_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL getMaxLine_core_inst_sin_out_1_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_sin_out_1_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_sin_out_1_rsci_data_out_d : STD_LOGIC_VECTOR (53 DOWNTO
      0);
  SIGNAL getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_a : STD_LOGIC_VECTOR
      (17 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_b : STD_LOGIC_VECTOR
      (26 DOWNTO 0);
  SIGNAL getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z : STD_LOGIC_VECTOR
      (43 DOWNTO 0);

BEGIN
  cos_out_1_rsc_comp : work.ram_sync_dualrw_be_pkg.ram_sync_dualRW_be
    GENERIC MAP(
      ram_id => 22,
      words => 180,
      width => 27,
      addr_width => 8,
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
      data_in => cos_out_1_rsc_comp_data_in,
      addr => cos_out_1_rsc_comp_addr,
      re => cos_out_1_rsc_comp_re,
      we => cos_out_1_rsc_comp_we,
      data_out => cos_out_1_rsc_comp_data_out,
      clk => clk,
      a_rst => '1',
      s_rst => rst,
      en => cos_out_1_rsc_en
    );
  cos_out_1_rsc_comp_data_in <= cos_out_1_rsc_data_in;
  cos_out_1_rsc_comp_addr <= cos_out_1_rsc_addr;
  cos_out_1_rsc_comp_re <= cos_out_1_rsc_re;
  cos_out_1_rsc_comp_we <= cos_out_1_rsc_we;
  cos_out_1_rsc_data_out <= cos_out_1_rsc_comp_data_out;

  sin_out_1_rsc_comp : work.ram_sync_dualrw_be_pkg.ram_sync_dualRW_be
    GENERIC MAP(
      ram_id => 23,
      words => 180,
      width => 27,
      addr_width => 8,
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
      data_in => sin_out_1_rsc_comp_data_in,
      addr => sin_out_1_rsc_comp_addr,
      re => sin_out_1_rsc_comp_re,
      we => sin_out_1_rsc_comp_we,
      data_out => sin_out_1_rsc_comp_data_out,
      clk => clk,
      a_rst => '1',
      s_rst => rst,
      en => sin_out_1_rsc_en
    );
  sin_out_1_rsc_comp_data_in <= sin_out_1_rsc_data_in;
  sin_out_1_rsc_comp_addr <= sin_out_1_rsc_addr;
  sin_out_1_rsc_comp_re <= sin_out_1_rsc_re;
  sin_out_1_rsc_comp_we <= sin_out_1_rsc_we;
  sin_out_1_rsc_data_out <= sin_out_1_rsc_comp_data_out;

  cos_out_1_rsci : getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT MAP(
      en => cos_out_1_rsc_en,
      data_out => cos_out_1_rsci_data_out,
      we => cos_out_1_rsci_we,
      re => cos_out_1_rsci_re,
      addr => cos_out_1_rsci_addr,
      data_in => cos_out_1_rsci_data_in,
      data_in_d => cos_out_1_rsci_data_in_d_1,
      addr_d => cos_out_1_rsci_addr_d_1,
      re_d => cos_out_1_rsci_re_d_1,
      we_d => cos_out_1_rsci_we_d_1,
      data_out_d => cos_out_1_rsci_data_out_d_1,
      en_d => cos_out_1_rsci_en_d
    );
  cos_out_1_rsci_data_out <= cos_out_1_rsc_data_out;
  cos_out_1_rsc_we <= cos_out_1_rsci_we;
  cos_out_1_rsc_re <= cos_out_1_rsci_re;
  cos_out_1_rsc_addr <= cos_out_1_rsci_addr;
  cos_out_1_rsc_data_in <= cos_out_1_rsci_data_in;
  cos_out_1_rsci_data_in_d_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000000")
      & cos_out_1_rsci_data_in_d;
  cos_out_1_rsci_addr_d_1 <= STD_LOGIC_VECTOR'( "00000000") & cos_out_1_rsci_addr_d;
  cos_out_1_rsci_re_d_1 <= cos_out_1_rsci_re_d;
  cos_out_1_rsci_we_d_1 <= cos_out_1_rsci_we_d;
  cos_out_1_rsci_data_out_d <= cos_out_1_rsci_data_out_d_1;

  sin_out_1_rsci : getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT MAP(
      en => sin_out_1_rsc_en,
      data_out => sin_out_1_rsci_data_out,
      we => sin_out_1_rsci_we,
      re => sin_out_1_rsci_re,
      addr => sin_out_1_rsci_addr,
      data_in => sin_out_1_rsci_data_in,
      data_in_d => sin_out_1_rsci_data_in_d_1,
      addr_d => sin_out_1_rsci_addr_d,
      re_d => sin_out_1_rsci_re_d_1,
      we_d => sin_out_1_rsci_we_d_1,
      data_out_d => sin_out_1_rsci_data_out_d_1,
      en_d => cos_out_1_rsci_en_d
    );
  sin_out_1_rsci_data_out <= sin_out_1_rsc_data_out;
  sin_out_1_rsc_we <= sin_out_1_rsci_we;
  sin_out_1_rsc_re <= sin_out_1_rsci_re;
  sin_out_1_rsc_addr <= sin_out_1_rsci_addr;
  sin_out_1_rsc_data_in <= sin_out_1_rsci_data_in;
  sin_out_1_rsci_data_in_d_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000000")
      & sin_out_1_rsci_data_in_d;
  sin_out_1_rsci_addr_d <= STD_LOGIC_VECTOR'( "00000000") & cos_out_1_rsci_addr_d;
  sin_out_1_rsci_re_d_1 <= sin_out_1_rsci_re_d;
  sin_out_1_rsci_we_d_1 <= sin_out_1_rsci_we_d;
  sin_out_1_rsci_data_out_d <= sin_out_1_rsci_data_out_d_1;

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
      cos_out_1_rsci_data_in_d => getMaxLine_core_inst_cos_out_1_rsci_data_in_d,
      cos_out_1_rsci_addr_d => getMaxLine_core_inst_cos_out_1_rsci_addr_d,
      cos_out_1_rsci_re_d => getMaxLine_core_inst_cos_out_1_rsci_re_d,
      cos_out_1_rsci_we_d => getMaxLine_core_inst_cos_out_1_rsci_we_d,
      cos_out_1_rsci_data_out_d => getMaxLine_core_inst_cos_out_1_rsci_data_out_d,
      cos_out_1_rsci_en_d => cos_out_1_rsci_en_d,
      sin_out_1_rsci_data_in_d => getMaxLine_core_inst_sin_out_1_rsci_data_in_d,
      sin_out_1_rsci_re_d => getMaxLine_core_inst_sin_out_1_rsci_re_d,
      sin_out_1_rsci_we_d => getMaxLine_core_inst_sin_out_1_rsci_we_d,
      sin_out_1_rsci_data_out_d => getMaxLine_core_inst_sin_out_1_rsci_data_out_d,
      T_LINE_if_if_dividend1_mul_cmp_a => getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_a,
      T_LINE_if_if_dividend1_mul_cmp_b => getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_b,
      T_LINE_if_if_dividend1_mul_cmp_z => getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z
    );
  x1_rsc_dat <= getMaxLine_core_inst_x1_rsc_dat;
  y1_rsc_dat <= getMaxLine_core_inst_y1_rsc_dat;
  x2_rsc_dat <= getMaxLine_core_inst_x2_rsc_dat;
  y2_rsc_dat <= getMaxLine_core_inst_y2_rsc_dat;
  getMaxLine_core_inst_acc_rsc_dat <= acc_rsc_dat;
  cos_out_1_rsci_data_in_d <= getMaxLine_core_inst_cos_out_1_rsci_data_in_d;
  cos_out_1_rsci_addr_d <= getMaxLine_core_inst_cos_out_1_rsci_addr_d;
  cos_out_1_rsci_re_d <= getMaxLine_core_inst_cos_out_1_rsci_re_d;
  cos_out_1_rsci_we_d <= getMaxLine_core_inst_cos_out_1_rsci_we_d;
  getMaxLine_core_inst_cos_out_1_rsci_data_out_d <= cos_out_1_rsci_data_out_d;
  sin_out_1_rsci_data_in_d <= getMaxLine_core_inst_sin_out_1_rsci_data_in_d;
  sin_out_1_rsci_re_d <= getMaxLine_core_inst_sin_out_1_rsci_re_d;
  sin_out_1_rsci_we_d <= getMaxLine_core_inst_sin_out_1_rsci_we_d;
  getMaxLine_core_inst_sin_out_1_rsci_data_out_d <= sin_out_1_rsci_data_out_d;
  T_LINE_if_if_dividend1_mul_cmp_a <= getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_a;
  T_LINE_if_if_dividend1_mul_cmp_b <= getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_b;
  getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z <= STD_LOGIC_VECTOR(CONV_UNSIGNED(SIGNED'(
      SIGNED(T_LINE_if_if_dividend1_mul_cmp_a) * SIGNED(T_LINE_if_if_dividend1_mul_cmp_b)),
      44));

END v25;

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

ARCHITECTURE v25 OF houghTransform IS
  -- Default Constants
  CONSTANT PWR : STD_LOGIC := '1';

  -- Interconnect Declarations
  SIGNAL widthIn_rsci_idat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL heightIn_rsci_idat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_in_d : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL acc_tmp_rsci_addr_d : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsci_data_out_d : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsci_en_d : STD_LOGIC;
  SIGNAL cos_out_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL cos_out_rsci_addr_d : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL cos_out_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsci_data_out_d : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_rsci_en_d : STD_LOGIC;
  SIGNAL sin_out_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO 0);
  SIGNAL sin_out_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsci_data_out_d : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL acc_tmp_rsc_en : STD_LOGIC;
  SIGNAL acc_tmp_rsc_data_out : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsc_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_addr : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsc_data_in : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL cos_out_rsc_en : STD_LOGIC;
  SIGNAL cos_out_rsc_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_rsc_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsc_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsc_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_rsc_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_rsc_en : STD_LOGIC;
  SIGNAL sin_out_rsc_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_rsc_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsc_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsc_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_rsc_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);

  SIGNAL widthIn_rsci_dat : STD_LOGIC_VECTOR (10 DOWNTO 0);
  SIGNAL widthIn_rsci_idat_1 : STD_LOGIC_VECTOR (10 DOWNTO 0);

  SIGNAL heightIn_rsci_dat : STD_LOGIC_VECTOR (9 DOWNTO 0);
  SIGNAL heightIn_rsci_idat_1 : STD_LOGIC_VECTOR (9 DOWNTO 0);

  SIGNAL acc_tmp_rsc_comp_data_in : STD_LOGIC_VECTOR (31 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_addr : STD_LOGIC_VECTOR (37 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL acc_tmp_rsc_comp_data_out : STD_LOGIC_VECTOR (31 DOWNTO 0);

  SIGNAL cos_out_rsc_comp_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_rsc_comp_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_rsc_comp_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsc_comp_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsc_comp_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);

  SIGNAL sin_out_rsc_comp_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_rsc_comp_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_rsc_comp_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsc_comp_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsc_comp_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);

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

  COMPONENT houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT(
      en : OUT STD_LOGIC;
      data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      en_d : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL cos_out_rsci_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_rsci_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsci_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsci_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_rsci_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_rsci_data_in_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL cos_out_rsci_addr_d_1 : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL cos_out_rsci_re_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsci_we_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL cos_out_rsci_data_out_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);

  COMPONENT houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT(
      en : OUT STD_LOGIC;
      data_out : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      we : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      re : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      addr : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
      data_in : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      data_in_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      addr_d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
      re_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      we_d : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
      data_out_d : OUT STD_LOGIC_VECTOR (53 DOWNTO 0);
      en_d : IN STD_LOGIC
    );
  END COMPONENT;
  SIGNAL sin_out_rsci_data_out : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_rsci_we : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsci_re : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsci_addr : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_rsci_data_in : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_rsci_data_in_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);
  SIGNAL sin_out_rsci_addr_d : STD_LOGIC_VECTOR (15 DOWNTO 0);
  SIGNAL sin_out_rsci_re_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsci_we_d_1 : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL sin_out_rsci_data_out_d_1 : STD_LOGIC_VECTOR (53 DOWNTO 0);

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
      acc_tmp_rsci_addr_d : OUT STD_LOGIC_VECTOR (37 DOWNTO 0);
      acc_tmp_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      acc_tmp_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      acc_tmp_rsci_data_out_d : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
      acc_tmp_rsci_en_d : OUT STD_LOGIC;
      cos_out_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      cos_out_rsci_addr_d : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
      cos_out_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      cos_out_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      cos_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0);
      cos_out_rsci_en_d : OUT STD_LOGIC;
      sin_out_rsci_data_in_d : OUT STD_LOGIC_VECTOR (26 DOWNTO 0);
      sin_out_rsci_re_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      sin_out_rsci_we_d : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
      sin_out_rsci_data_out_d : IN STD_LOGIC_VECTOR (53 DOWNTO 0)
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
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_addr_d : STD_LOGIC_VECTOR (37 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_acc_tmp_rsci_data_out_d : STD_LOGIC_VECTOR (31
      DOWNTO 0);
  SIGNAL houghTransform_core_inst_cos_out_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_cos_out_rsci_addr_d : STD_LOGIC_VECTOR (7 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_cos_out_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_cos_out_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_cos_out_rsci_data_out_d : STD_LOGIC_VECTOR (53
      DOWNTO 0);
  SIGNAL houghTransform_core_inst_sin_out_rsci_data_in_d : STD_LOGIC_VECTOR (26 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_sin_out_rsci_re_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_sin_out_rsci_we_d : STD_LOGIC_VECTOR (1 DOWNTO
      0);
  SIGNAL houghTransform_core_inst_sin_out_rsci_data_out_d : STD_LOGIC_VECTOR (53
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

  cos_out_rsc_comp : work.ram_sync_dualrw_be_pkg.ram_sync_dualRW_be
    GENERIC MAP(
      ram_id => 14,
      words => 180,
      width => 27,
      addr_width => 8,
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
      data_in => cos_out_rsc_comp_data_in,
      addr => cos_out_rsc_comp_addr,
      re => cos_out_rsc_comp_re,
      we => cos_out_rsc_comp_we,
      data_out => cos_out_rsc_comp_data_out,
      clk => clk,
      a_rst => '1',
      s_rst => rst,
      en => cos_out_rsc_en
    );
  cos_out_rsc_comp_data_in <= cos_out_rsc_data_in;
  cos_out_rsc_comp_addr <= cos_out_rsc_addr;
  cos_out_rsc_comp_re <= cos_out_rsc_re;
  cos_out_rsc_comp_we <= cos_out_rsc_we;
  cos_out_rsc_data_out <= cos_out_rsc_comp_data_out;

  sin_out_rsc_comp : work.ram_sync_dualrw_be_pkg.ram_sync_dualRW_be
    GENERIC MAP(
      ram_id => 15,
      words => 180,
      width => 27,
      addr_width => 8,
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
      data_in => sin_out_rsc_comp_data_in,
      addr => sin_out_rsc_comp_addr,
      re => sin_out_rsc_comp_re,
      we => sin_out_rsc_comp_we,
      data_out => sin_out_rsc_comp_data_out,
      clk => clk,
      a_rst => '1',
      s_rst => rst,
      en => sin_out_rsc_en
    );
  sin_out_rsc_comp_data_in <= sin_out_rsc_data_in;
  sin_out_rsc_comp_addr <= sin_out_rsc_addr;
  sin_out_rsc_comp_re <= sin_out_rsc_re;
  sin_out_rsc_comp_we <= sin_out_rsc_we;
  sin_out_rsc_data_out <= sin_out_rsc_comp_data_out;

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
  acc_tmp_rsci_addr_d_1 <= acc_tmp_rsci_addr_d;
  acc_tmp_rsci_re_d_1 <= acc_tmp_rsci_re_d;
  acc_tmp_rsci_we_d_1 <= acc_tmp_rsci_we_d;
  acc_tmp_rsci_data_out_d <= acc_tmp_rsci_data_out_d_1;

  cos_out_rsci : houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT MAP(
      en => cos_out_rsc_en,
      data_out => cos_out_rsci_data_out,
      we => cos_out_rsci_we,
      re => cos_out_rsci_re,
      addr => cos_out_rsci_addr,
      data_in => cos_out_rsci_data_in,
      data_in_d => cos_out_rsci_data_in_d_1,
      addr_d => cos_out_rsci_addr_d_1,
      re_d => cos_out_rsci_re_d_1,
      we_d => cos_out_rsci_we_d_1,
      data_out_d => cos_out_rsci_data_out_d_1,
      en_d => cos_out_rsci_en_d
    );
  cos_out_rsci_data_out <= cos_out_rsc_data_out;
  cos_out_rsc_we <= cos_out_rsci_we;
  cos_out_rsc_re <= cos_out_rsci_re;
  cos_out_rsc_addr <= cos_out_rsci_addr;
  cos_out_rsc_data_in <= cos_out_rsci_data_in;
  cos_out_rsci_data_in_d_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000000") &
      cos_out_rsci_data_in_d;
  cos_out_rsci_addr_d_1 <= STD_LOGIC_VECTOR'( "00000000") & cos_out_rsci_addr_d;
  cos_out_rsci_re_d_1 <= cos_out_rsci_re_d;
  cos_out_rsci_we_d_1 <= cos_out_rsci_we_d;
  cos_out_rsci_data_out_d <= cos_out_rsci_data_out_d_1;

  sin_out_rsci : houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    PORT MAP(
      en => sin_out_rsc_en,
      data_out => sin_out_rsci_data_out,
      we => sin_out_rsci_we,
      re => sin_out_rsci_re,
      addr => sin_out_rsci_addr,
      data_in => sin_out_rsci_data_in,
      data_in_d => sin_out_rsci_data_in_d_1,
      addr_d => sin_out_rsci_addr_d,
      re_d => sin_out_rsci_re_d_1,
      we_d => sin_out_rsci_we_d_1,
      data_out_d => sin_out_rsci_data_out_d_1,
      en_d => cos_out_rsci_en_d
    );
  sin_out_rsci_data_out <= sin_out_rsc_data_out;
  sin_out_rsc_we <= sin_out_rsci_we;
  sin_out_rsc_re <= sin_out_rsci_re;
  sin_out_rsc_addr <= sin_out_rsci_addr;
  sin_out_rsc_data_in <= sin_out_rsci_data_in;
  sin_out_rsci_data_in_d_1 <= STD_LOGIC_VECTOR'( "000000000000000000000000000") &
      sin_out_rsci_data_in_d;
  sin_out_rsci_addr_d <= STD_LOGIC_VECTOR'( "00000000") & cos_out_rsci_addr_d;
  sin_out_rsci_re_d_1 <= sin_out_rsci_re_d;
  sin_out_rsci_we_d_1 <= sin_out_rsci_we_d;
  sin_out_rsci_data_out_d <= sin_out_rsci_data_out_d_1;

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
      acc_tmp_rsci_en_d => acc_tmp_rsci_en_d,
      cos_out_rsci_data_in_d => houghTransform_core_inst_cos_out_rsci_data_in_d,
      cos_out_rsci_addr_d => houghTransform_core_inst_cos_out_rsci_addr_d,
      cos_out_rsci_re_d => houghTransform_core_inst_cos_out_rsci_re_d,
      cos_out_rsci_we_d => houghTransform_core_inst_cos_out_rsci_we_d,
      cos_out_rsci_data_out_d => houghTransform_core_inst_cos_out_rsci_data_out_d,
      cos_out_rsci_en_d => cos_out_rsci_en_d,
      sin_out_rsci_data_in_d => houghTransform_core_inst_sin_out_rsci_data_in_d,
      sin_out_rsci_re_d => houghTransform_core_inst_sin_out_rsci_re_d,
      sin_out_rsci_we_d => houghTransform_core_inst_sin_out_rsci_we_d,
      sin_out_rsci_data_out_d => houghTransform_core_inst_sin_out_rsci_data_out_d
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
  cos_out_rsci_data_in_d <= houghTransform_core_inst_cos_out_rsci_data_in_d;
  cos_out_rsci_addr_d <= houghTransform_core_inst_cos_out_rsci_addr_d;
  cos_out_rsci_re_d <= houghTransform_core_inst_cos_out_rsci_re_d;
  cos_out_rsci_we_d <= houghTransform_core_inst_cos_out_rsci_we_d;
  houghTransform_core_inst_cos_out_rsci_data_out_d <= cos_out_rsci_data_out_d;
  sin_out_rsci_data_in_d <= houghTransform_core_inst_sin_out_rsci_data_in_d;
  sin_out_rsci_re_d <= houghTransform_core_inst_sin_out_rsci_re_d;
  sin_out_rsci_we_d <= houghTransform_core_inst_sin_out_rsci_we_d;
  houghTransform_core_inst_sin_out_rsci_data_out_d <= sin_out_rsci_data_out_d;

END v25;

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

ARCHITECTURE v25 OF Hough_Algorithm_HW_1296_864 IS
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
      rscid => 25,
      width => 16,
      sz_width => 1,
      fifo_sz => 8,
      log2_sz => 3,
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
END v25;



