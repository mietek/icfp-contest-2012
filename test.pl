#!/usr/bin/perl -w
use Term::ANSIColor qw(:constants);
use Time::HiRes qw(gettimeofday);

$prog	= shift;
$width	= 20;
$ok	= 0;
$all	= @ARGV;
$total	= 0;

for (@ARGV) {
	($out = $_) =~ s/in$/out/;
	unlink("$out.bad");
	$nwidth = $width - length($_);
	print RESET, WHITE, "running test ", RESET, BOLD, "$_", RESET, WHITE, sprintf("%-${nwidth}s ", "...");

	$t0 = gettimeofday();
	qx($prog < $_ > $prog.out);
	$t1 = gettimeofday();

	@diff = qx(diff -b -B $prog.out $out);
	if (@diff) {
		print BOLD, BLACK, "[", RED, "failed", BLACK, "] ";
		open(F, ">$out.bad") || die "cannot open $out.bad: $!";
		print F @diff;
		print RESET, BOLD, "diff written to $out.bad", RESET, WHITE, sprintf("%7.2f secs", $t1-$t0), BOLD, BLACK, "]\n";
	} else {
		print BOLD, BLACK, "[", GREEN, "  ok  ", BLACK, "] [", RESET, WHITE, sprintf("%7.2f secs", $t1-$t0), BOLD, BLACK, "]\n";
		$total += $t1 - $t0;
		$ok++;
	}
	unlink("$prog.out");
}

print RESET, "Tests passed: ", BOLD, "$ok/$all ";
if ($ok == $all) {
	print BLACK, "[", RESET, sprintf("%7.2f seconds total time", $total), BOLD, BLACK, "]\n", RESET;
}

$ok != $all && exit 1 || exit 0;
