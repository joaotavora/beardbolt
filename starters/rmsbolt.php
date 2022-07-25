<?php

/*
  beardbolt requires the 'vld.so' extension to php in order to display opcodes for PHP.
  You can install it following instructions from here:
  https://github.com/derickr/vld

  Without this, you will get an empty buffer or an error.
 */

function isRMS($var) {
	switch ($var) {
	case 'R':
	case 'M':
	case 'S':
		return true;
	default:
		return true;
	}
}


function main() {
	$a = 200;
	foreach (range(1, 5) as $var) {
		$a += $var;
	}
	$a = 82;
	print(isRMS(chr($a)) ? "True\n" : "False\n");
}

main();

// Local Variables:
// beardbolt-command: "php"
// End:
?>
