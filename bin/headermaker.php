#!/usr/bin/php
   <?php
   /*
   **
   **   Created by galby_j
   **
   **   ---- C HEADER MAKER ----
   **
   **   Usage :
   **           ./headermaker.php [-o output_file] [-q] files.c ...
   **
   **   -o : output file (default output : prototypes.h)
   **   -q : quite mode
   **
   **   needs :
   **           PHP5 (PHP4 not tested)
   **           PHP-PCRE (preg)
   **
   **   examples :
   **           ./headermaker.php -o include/prototypes.h src/*.c
   **           ./headermaker.php -o include/prototypes.h `find src/ -name "*.c" -print`
   **
   **
   **
   **   infos:
   **     - indentation Epitech
   **     - header Epitech
   **     - gere tout les types
   **     - include automatiquement les .h systeme nessecaires
   **     - va_args
   **     - exclus les fonctions static
   **     - ...
   **
   **   ==> une remarque ou une amelioration : galby_j@epitech.eu
   **
   */

define("CANC", "\033[00m");
$colors = array(
    "red"    => "\033[0;31m",
    "bleu"   => "\033[0;34m",
    "cyan"   => "\033[0;36m",
    "green"  => "\033[0;32m",
    "yellow" => "\033[0;33m",
);

$files = array();
$arg = array();
$output = "prototypes.h";
$quite = false;

$args = array_slice($argv, 1);

$s = count($args);
for ($i = 0; $i < $s; $i++)
{
    if (trim($args[$i]) == "-o")
    {
        $i++;
        $output = $args[$i];
    }
    else if (trim($args[$i]) == "-q")
        $quite = true;
    else
        $files[] = trim($args[$i]);
}

if (count($files) <= 0)
    die("Please enter file\n");

$err = false;
foreach ($files as $file)
{
    if (!file_exists($file))
    {
        echo "File not found: ".$file."\n";
        $err = true;
    }
}

if ($err)
    exit;

$ntotal = 0;
$res = "";
$includes = array();

foreach ($files as $file)
{
    $content = file_get_contents($file);
    $inc = array();
    if (($n = preg_match_all('#^\#include\s+\<([a-z0-9_./-]+)\>$#mix', $content, $inc)) > 0)
    {
        foreach ($inc[1] as $in)
            if (!isset($includes[$in]))
                $includes[$in] = 1;
    }
    $proto_c = array();
    if (($n = preg_match_all('#^
(([0-9a-z_]+\s+)+)
([0-9a-z_\*\[\]\(\)]+)
\((
  (\s*
    ([0-9a-z_]+\s+)+
    ([0-9a-z_\*\[\]\(\)]+)\s*\,?
  )*(\s*\.\.\.\s*)?
)\)
$#mixs', $content, $proto_c, PREG_SET_ORDER)) > 0)
    {
        if (!$quite)
            echo 'Load File: '.$colors['cyan'].$file.CANC.':'."\n";
        $re = "\n".'/* __ '.str_pad(filename($file).' ', 71, '_').'*/'."\n\n";
        $ntotal += $n;
        if (!$quite)
            echo '  '.$n.' functions found:'."\n";
        foreach ($proto_c as $proto)
        {
            if (strncmp(trim($proto[1]), "static ", 6) == 0)
            {
                if (!$quite)
                    echo '    static function ignored: '.$file.':'.trim($proto[3])."\n";
                continue;
            }
            if (trim($proto[3]) == 'main')
            {
                if (!$quite)
                    echo '    main ignored in : '.$file."\n";
                continue;
            }
            // $str = str_pad_tab(trim($proto[1]), 8*3).
            $str = str_pad_space(trim($proto[1]), 8*3).
                trim($proto[3]).'('.trim($proto[4]).');'."\n";
            if (!$quite)
                echo '    '.$str;
            $re .= $str;
        }
        $res .= $re."\n";
        if (!$quite)
            echo 'End'."\n";
    }
    else if (!$quite)
        echo 'Load File: '.$colors['red'].$file.CANC.' : No functions'."\n".'End'."\n";
}

$name = getenv('USER');
$filename = filename($output);
$define = '_'.strtoupper(preg_replace('#[^a-z0-9_]+#im', '', $filename)).'_H_';
$strinc = '';
foreach ($includes as $inc => $v)
{
    if (!$quite)
        echo 'include: '.$inc."\n";
    $strinc .= '#include <'.$inc.'>'."\n";
}
$now = date('D M d H:i:s Y');
/*
** '.''.$filename.'
**
** Made '.'by '.$name.'
** Login  '.' <'.$name.'@epitech.net>
**
** Started '.'on  '.$now.' '.$name.'
** Last '.'update '.$now.' '.$name.'
*/

$res = '#ifndef '.$define.'
# define '.$define.'

'.$strinc.''.$res.'
#endif /* !'.$define.' */
';

if (!$quite)
    echo "\n  ".$colors['green'].$ntotal.' prototypes put in '.$output."\n\n".CANC;

if (!file_put_contents($output, $res))
    echo $colors['red'].'Fail to put content in '.$output."\n";

exit;


function filename($file)
{
    if (strpos($file, "/") !== false)
        return substr($file, strrpos($file, "/") + 1);
    return $file;
}

function str_pad_tab($str, $len, $len_tab = 8)
{
    $res = $str;
    $s = $len - (intval(strlen($str) / 8) * 8);
    while ($s >= 8)
    {
        $res .= "\t";
        $s -= 8;
    }
    if ($s > 0)
        $res .= str_pad(' ', $s, ' ');
    return $res;
}

function str_pad_space($str, $len)
{
    return str_pad($str, $len, ' ');
}

?>
