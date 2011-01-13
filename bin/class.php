#!/usr/bin/php
<?php

$ext_hpp = "h";
$ext_cpp = "cpp";

/*
	0 = no changes;
	1 = lower;
	2 = first letter lower;
*/
$lower_file = 0;

if ($argv[1] == '-0' || $argv[1] == '-1' || $argv[1] == '-2')
{
   $lower_file = -intval($argv[1]);
   array_shift($argv);
   $argc--;
}

for ($i = 1; $i < $argc; $i++)
	createClass($argv[$i]);

exit;

function createClass($name)
{
	global $ext_hpp, $ext_cpp;

$cpp = '#include <iostream>

#include "'.lower($name).'.'.$ext_hpp.'"

'.$name.'::'.$name.'()
{
}
'.$name.'::~'.$name.'()
{
}

'.$name.'&			'.$name.'::operator=(const '.$name.'& from)
{ return (*this); }
'.$name.'::'.$name.'(const '.$name.'& from)
{ }
';

$hpp = '#ifndef _'.strtoupper($name).'_'.strtoupper($ext_hpp).'_
# define _'.strtoupper($name).'_'.strtoupper($ext_hpp).'_

class '.$name.'
{
public:
	'.$name.'();
	virtual ~'.$name.'();

protected:
private:
	'.$name.'(const '.$name.'& from);
	'.$name.'&		operator=(const '.$name.'& from);
};

#endif /* _'.strtoupper($name).'_'.strtoupper($ext_hpp).'_ */
';

file_put_contents(lower($name).'.'.$ext_cpp, $cpp);
file_put_contents(lower($name).'.'.$ext_hpp, $hpp);

}

function lower($str)
{
	global $lower_file;

	if ($lower_file == 1)
		$str = strtolower($str);
	else if ($lower_file == 2)
		$str = lcfirst($str);
	return ($str);
}

?>
