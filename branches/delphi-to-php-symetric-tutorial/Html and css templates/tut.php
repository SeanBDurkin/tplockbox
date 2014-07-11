<!DOCTYPE html>
<html lang="en">
 <head>
	<META HTTP-EQUIV="CONTENT-TYPE" CONTENT="text/html; charset=UTF-8">
	<META NAME="ROBOTS" CONTENT="NOINDEX,NOFOLLOW,NOARCHIVE" />
	<META NAME="DESCRIPTION" CONTENT="Tutorial and demostration page for Delphi (TurboPower LockBox3) to PHP symetric cryptography." />
	<META NAME="AUTHOR" CONTENT="Sean B. Durkin">
	<META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE">
    <META NAME="KEYWORDS" CONTENT="cryptography,delphi,php,turbopower,lockbox,lockbox3">
	<title>Delphi-to-PHP Cryptography Tutorial</title>
    <link rel="stylesheet" media="screen and (min-device-width:  721px)" href="tut.css" /><!-- <== Desktop. -->  	
    <link rel="stylesheet" media="screen and (max-device-width:  720px)" href="tut-phone.css" / --><!-- <== Phone and tablet. -->  
 <head>

<body>
<?php
error_reporting(E_ALL | E_COMPILE_ERROR);
$password = $_GET["password"];
$ciphertext_trans = $_GET["ciphertext-trans"];
$rawInputCiphertext = $_GET["ciphertext"];
$chain = $_GET["chain"];
$cipherIn = $_GET["cipherIn"];

function radioChecked($param,$value,$isDefault)
{
  $echo = (($param == $value) or ($isDefault and ($param == ''))) ? ' checked="checked" ' : '';
  echo $echo;
  return $echo != '';  
}
?>

<nav>
  <div class="nav-bg"></div>
  <div class="nav-content">
    <h1><a href="http://lockbox.seanbdurkin.id.au/tiki-index.php?page=Delphi-to-PHP+Tutorial+project+home" title="Go to The Delphi-to-PHP project home"><span class="hidden">Project home</span></a></h1>
    <dl>
      <dt>Date created</dt>
      <dd><time datetime="2012-11-29">29-Nov-2012</time></dd>
    </dl>
    <dl>
      <dt>Date last modified</dt>
      <dd><time datetime="2012-12-02">2-Dec-2012</time></dd>
    </dl>
  </div>	
</nav>

<h1>Decrypt with PHP from Delphi (TurboPower LockBox3)</h1>
<form id="plainForm" class="floatbox" action="">
  <fieldset>
    <legend>Crypto data</legend>
	<label class="first" for="password">Password (UTF-8)
	  <input id="password" name="password" type="text" placeholder="Enter TCodec password" value="<?php echo htmlspecialchars($password) ?>" />
	</label>			
    <fieldset class="radio">
	  <legend>Transport encoding</legend>
	    <label for="ciphertext-trans-base64">
		  <input id="ciphertext-trans-base64" name="ciphertext-trans" type="radio" value="base64" 
		  <?php radioChecked($ciphertext_trans,'base64',True); ?> />base64
		</label>
	    <label for="ciphertext-trans-hex">
		  <input id="ciphertext-trans-hex" name="ciphertext-trans" type="radio" value="hex" 
		  <?php radioChecked($ciphertext_trans,'hex',False); ?> />hex
		</label>
	</fieldset>				
	<label for="ciphertext">Ciphertext (transport encoded)
	  <input id="ciphertext" name="ciphertext" type="text" placeholder="Paste ciphertext here" value="<?php echo htmlspecialchars($rawInputCiphertext) ?>" />
	</label>			
  </fieldset>
  <fieldset>
    <legend>Options</legend>
    <fieldset class="radio">
	  <legend>Chaining mode</legend>
        <label for="chain-cfb">
		  <input id="chain-cfb" name="chain" type="radio" 
            value="CFB" <?php radioChecked($chain,'CFB',True); ?> />CFB
        </label>
        <label for="chain-cbc">
		  <input id="chain-cbc" name="chain" type="radio"
            value="CBC" <?php radioChecked($chain,'CBC',False); ?> />CBC
        </label>
        <label for="chain-ecb">
		  <input id="chain-ecb" name="chain" type="radio"
            value="ECB" <?php radioChecked($chain,'ECB',False); ?> />ECB
        </label>
    </fieldset>				

    <fieldset class="radio">
	  <legend>Cipher</legend>
        <label for="aes-128">
		  <input id="aes-128" name="cipherIn" type="radio" 
            value="AES-128" <?php radioChecked($cipherIn,'AES-128',True); ?> />AES-128
        </label>
		<!-- Extend here with more ciphers as required. Note: PHP does not support AES-256. -->
    </fieldset>				
	
	</fieldset>				
  <input class="submit" type="submit" value="Decrypt" />
</form>
<?php if ($chain) { ?>
<?php
function purgeWhiteSpace($sparseHex)
{
    return preg_replace('/\s+/', '', $sparseHex);
}
function expandWithWhiteSpace($compactHex)
{
// TODO: Insert white space for visual benefit. Bunch the brown spaces
//  into words of 6 characters, and then separate words with a single space.
//  Between every 10th word and 11th word, use a new-line ($0D) instead of space.
//  Assume that $compactHex ONLY consists of characters 0..9 and A..F .
    return $compactHex;
}
function displayableMultiline($str)
{
// TODO: Assume $str ONLY consists of characters whose code-points are below
//  137. Insert '<br />' before each $0D character.
    return $str;
}
function hexToStr($hex)
{
    $hex2 = purgeWhiteSpace( $hex);
	$str='';
	for ($i=0; $i < strlen($hex2)-1; $i+=2)
	{
		$str .= chr(hexdec($hex2[$i].$hex2[$i+1]));
	}
	return $str;
}	
function strToHex($str)
{
    $hex='';
    for ($i=0; $i < strlen($str); $i++)
    {	    
        $addend = dechex(ord($str[$i]));
		if (strlen($addend) < 2)
		  $addend = '0' . $addend;
        $hex .= $addend;
    }
    return $hex;
}

$normalisedRawCiphertext = purgeWhiteSpace( $rawInputCiphertext);
if ($ciphertext_trans == 'base64')
{ 
  $ciphertext = base64_decode( $normalisedRawCiphertext);
}
else
{
  $ciphertext = hexToStr( $normalisedRawCiphertext);
}

if ($cipherIn == 'AES-128')
{
  $cipher = MCRYPT_RIJNDAEL_128;
  $cipherName = 'AES-128';
}
else
{
  // Extend here with more ciphers as required. Note: PHP does not support AES-256.
  $cipher = MCRYPT_RIJNDAEL_128; // Example only.
  $cipherName = '???';           // Example only.
}

if ($chain == 'CFB')
    $mode = 'ncfb';  // Proper block-mode CFB. There is no constant for this.
  else if ($chain == 'CBC')	
    $mode = MCRYPT_MODE_CBC;
  else	
    $mode = MCRYPT_MODE_ECB;

$blockSize = mcrypt_get_block_size( $cipher, $mode);
$keySize = mcrypt_get_key_size( $cipher, $mode);

// Work-around PHP bugs.
if (($cipher == MCRYPT_RIJNDAEL_128) and ($keySize == 32))
  { $keySize = 16; }   // AES-128 key size is 16 bytes.
if (($cipher == MCRYPT_RIJNDAEL_256) and ($blockSize == 32))
  { $blockSize = 16; } // AES-256 block size is 16 bytes.
  
$ivSize = $blockSize; // Always. mcrypt_get_iv_size() is pointless.

if ($chain == 'ECB')
{
    $iv = str_pad( 'NOT USED', 16, chr(0));
    // $ciphertext unchanged.
}
else
{
    $iv = substr( $ciphertext, 0, 8);
    $iv = str_pad( $iv, $ivSize, chr(0));
    $ciphertext = substr( $ciphertext, 8);
}
 
$ciphertextLen = strlen( $ciphertext);
if  (($ciphertextLen > 0) && ($ciphertextLen < $blockSize) && ($chain == 'CBC'))
 { $mode = MCRYPT_MODE_CFB; } // CFB 8-bit. This is NOT the same as CFB.

if (strlen($password)==$keySize)
  {
    $key = $password;
  }
else
  {
    $shaPassword = sha1( $password, True);
	for ($key = ''; strlen( $key) < $keySize; $key .= $shaPassword) {}
	$key = substr( $key, 0, $keySize);
  }  

$countBlocks = $ciphertextLen / $blockSize;
$countWholeBlocks = floor( $countBlocks); 
$isRound = $countBlocks == $countWholeBlocks; 
if ($isRound)
    {
	$lastBlockSize = 0;
	}
  else
    {
	$countBlocks = $countWholeBlocks + 1;
    $lastBlockSize = $ciphertextLen - ($countWholeBlocks * $blockSize);
    }	  
$isCipherStealing = ($mode == MCRYPT_MODE_CBC) && ($countWholeBlocks >= 1) && !$isRound;
if ($isCipherStealing)
    { // Reverse ciphertext stealing.
/* 
Ciphertext stealing algorithm - Encryption:
  Mix     := Enc( CV[N-2], X[N-2]);
  Steal   := Last( B-b, Mix);
  Recycle := X[N-1] + Steal;
  Y[N-2]  := Enc( CV[N-2], Recycle);
  Y[N-1]  := Head( b, Mix);
  
Ciphertext stealing algorithm - Decryption:
  Recycle := Dec( CV[N-2], Y[N-2]);
  Steal   := Last( B-b, Recycle);
  Mix     := Y[N-1] + Steal;
  X[N-2]  := Dec( CV[N-2], Mix);
  X[N-1]  := Head( b, Recycle);  
*/	
    // 1. Recycle := Dec( CV[N-2], Y[N-2]);
	$Recycle = mcrypt_decrypt ( $cipher, $key, substr( $ciphertext, 0, $countWholeBlocks * $blockSize), $mode, $iv);
	$reconUpToX_N_3 = substr( $Recycle, 0, ($countWholeBlocks - 1) * $blockSize); // X[0]..X{N-3]
	$Recycle = substr( $Recycle, ($countWholeBlocks - 1) * $blockSize, $blockSize);
		
	// 2. Steal := Last( B-b, Recycle);
	$Steal = substr( $Recycle, $lastBlockSize, $blockSize - $lastBlockSize);
	
	// 3. Mix := Y[N-1] + Steal;
	$Y_N1 = substr( $ciphertext, $countWholeBlocks * $blockSize, $lastBlockSize);
	$Mix = $Y_N1 . $Steal;
	
	// 4. X[N-2]  := Dec( CV[N-2], Mix);
	$reconUpToX_N_2 = mcrypt_decrypt ( $cipher, $key, substr( $ciphertext, 0, ($countWholeBlocks - 1) * $blockSize) . $Mix, $mode, $iv);
	
	// 5. X[N-1] := Head( b, Recycle);
	$reconX_N_1 = substr( $Recycle, 0, $lastBlockSize);
	
	// Putting it alltogether.
	$recon = $reconUpToX_N_2 . $reconX_N_1;
    }
  else
    { // Normal decyrption.
    $recon = mcrypt_decrypt ( $cipher, $key, $ciphertext, $mode, $iv);
    }
if (($chain == 'ECB') and ($recon != ''))
  { // Trim ECB padding.
  $last = strlen($recon);
  for ($l = strlen($recon); ($l >= 0) and (ord($recon[$l])==0); $l--)
    {$last = $l;}
  $recon = substr( $recon, 0, $last-1);
  }
?>
<hr />
<h2>Output</h2>
<h3>Summary2</h3>
<p>Cipher is <em><?php echo $cipherName; ?></em></p>
<p>Block size is <?php echo $blockSize; ?> bytes</p>
<?php if ($isRound) { ?>
  <p>Given ciphertext was a round <?php echo $countBlocks; ?> blocks long.</p>
<?php } else { ?>
  <p>Given ciphertext was a <?php echo $countWholeBlocks; ?> whole blocks long and <?php echo $lastBlockSize; ?> bytes in an odd block.</p>
<?php } ?>
<p>Key size is <?php echo $keySize; ?> bytes</p>
<p>Given chain mode was <em><?php echo $chain; ?></em></p>
<p>Given password was <em>&apos;<?php echo htmlspecialchars($password); ?>&apos;</em></p>
<p>Ciphertext as hex is...</p>
<code><?php echo '[' . $ciphertextLen . '] ' . displayableMultiline( expandWithWhiteSpace( strToHex( $ciphertext))); ?></code>
<p></p>
<p>Reconstructed plaintext message is <em>&apos;<?php echo htmlspecialchars( $recon); ?>&apos;</em></p>
<p></p>
<h2>Debug</h2>
<p>Key as hex is...</p>
<code><?php echo '[' . strlen($key) . '] ' . expandWithWhiteSpace( strToHex( $key)); ?></code>
<p>IV as hex is...</p>
<code><?php echo '[' . strlen($iv) . '] ' . expandWithWhiteSpace( strToHex( $iv)); ?></code>
<p>$countBlocks = <code><?php echo $countBlocks; ?></code></p>
<p>$countWholeBlocks = <code><?php echo $countWholeBlocks; ?></code></p>
<p>$isRound = <code><?php echo $isRound ? 'True' : 'False'; ?></code></p>
<p>$isCipherStealing = <code><?php echo $isCipherStealing ? 'True' : 'False'; ?></code></p>
<p>$lastBlockSize = <code><?php echo $lastBlockSize; ?></code></p>
<p>$Recycle = <code><?php echo '[' . strlen($Recycle) . '] ' . strToHex( $Recycle); ?></code></p>
<p>$recon X[0..N-3] = <code><?php echo '[' . strlen($reconUpToX_N_3) . '] ' . strToHex( $reconUpToX_N_3); ?></code></p>
<p>$Steal = <code><?php echo '[' . strlen($Steal) . '] ' . strToHex( $Steal); ?></code></p>
<p>$Mix = <code><?php echo '[' . strlen($Mix) . '] ' . strToHex( $Mix); ?></code></p>
<p>$recon X[0..N-2] = <code><?php echo '[' . strlen($reconUpToX_N_2) . '] ' . strToHex( $reconUpToX_N_2); ?></code></p>
<p>$recon X[N-1] = <code><?php echo '[' . strlen($reconX_N_1) . '] ' . strToHex( $reconX_N_1); ?></code></p>
<p>Reconstructed plaintext as hex is...</p>
<code><?php echo '[' . strlen($recon) . '] ' . expandWithWhiteSpace( strToHex( $recon)); ?></code>
<?php } ?>
</body> 
</html>