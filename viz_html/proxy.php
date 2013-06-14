<?php
$url = stripslashes(urldecode($_GET['url']));
$ch = curl_init();
curl_setopt ($ch, CURLOPT_URL, $url);
curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt ($ch, CURLOPT_FOLLOWLOCATION, true);
curl_setopt ($ch, CURLOPT_CONNECTTIMEOUT, 0);
$content = curl_exec($ch);
$t = curl_getinfo($ch, CURLINFO_CONTENT_TYPE);
curl_close($ch);
header('Content-Type: ' . $t);
echo $content;
?>