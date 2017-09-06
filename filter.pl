#!/usr/bin/env perl
use strict;
use warnings;

# experimental approach to filtering out all class attributes from
# HTML tags

# shortcuts/assumptions:

# - in HTML " must be quoted as &quot; so matching on class="…" should
#   not yield false positives from any plain text like paragraphs
# - the special [geshi lang="…"] marker is always ended by [/geshi], so just
#   skip everything in between

sub filter($)
{
    my $in = shift @_;
    my $out = '';

    while ($in ne '') {
	    my ($next_class_start, $next_class_end, $next_geshi_start) = (0, 0, 0);

	    if ($in =~ /\s+class=".*?"/) {
		$next_class_start = $-[0];
		$next_class_end = $+[0];
	    }
	    
	    if ($in =~ /\[geshi\s+lang=".*?"\]/) {
		$next_geshi_start = $-[0];
	    }
	    
	    if ($next_class_start + $next_geshi_start == 0) {
		# nothing left to do!
		$out .= $in;
		last;
	    }

	    if ($next_geshi_start == 0 or $next_class_start < $next_geshi_start) {
		# next comes class

		# copy over until class; skip class
		$out .= substr $in, 0, $next_class_start;
		$in = substr $in, $next_class_end;
	    }
	    else {
		# next comes GeSHi

		# copy over until GeSHi start
		$out .= substr $in, 0, $next_geshi_start;
		$in = substr $in, $next_geshi_start;

		# copy over until GeSHi end
		my $pos = index $in, '[/geshi]';
		$out .= substr $in, 0, $pos;
		$in = substr $in, $pos;
	    }
    }
    return $out;
}

# and now on to the tests to create a working filter
# TDD style!

use Test::More tests => 5;

subtest 'plain html is unchanged' => sub {
    is( filter('<p>some text</p>'), '<p>some text</p>' );
};

subtest 'class is removed' => sub {
    is( filter('<p class="a class">some text</p>'), '<p>some text</p>' );
};

subtest 'id attribute is kept' => sub {
    is( filter('<p class="a class" id="some id">some text</p>'), '<p id="some id">some text</p>' );
};

subtest 'no changes within GeSHi code block' => sub {
    my $given = <<'EOF'
<p class="removeme">some text</p>
[geshi lang="html"]
<p class="keepthis">inside a GeSHi tag, no substitutions are made</p>
[/geshi]
<p class="removeme">some more text</p>
EOF
	;
    
    my $expected = <<'EOF'
<p>some text</p>
[geshi lang="html"]
<p class="keepthis">inside a GeSHi tag, no substitutions are made</p>
[/geshi]
<p>some more text</p>
EOF
	;
    
    is( filter($given), $expected );
};

subtest 'GeSHi start tag inside a GeSHi block does nothing' => sub {
    my $given = <<'EOF'
<p class="removeme">some text</p>
[geshi lang="html"]
<p>Use [geshi lang="html"] to start a GeSHi block.</p>
<p class="important">Don't put the GeSHi end tag inside a GeSHi block.</p>
[/geshi]
<p class="removeme">some more text</p>
EOF
	;
    
    my $expected = <<'EOF'
<p>some text</p>
[geshi lang="html"]
<p>Use [geshi lang="html"] to start a GeSHi block.</p>
<p class="important">Don't put the GeSHi end tag inside a GeSHi block.</p>
[/geshi]
<p>some more text</p>
EOF
	;
    
    is( filter($given), $expected );
};
