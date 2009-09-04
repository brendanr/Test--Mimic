use strict;
use warnings;

use Data::Dump::Streamer qw<:undump>;
use Test::Mimic::Library qw<destringify>;

use Test::More 'no_plan';

# use test
BEGIN {
    unshift( @INC, 't' );
    use_ok( 'Test::Mimic::Recorder', {
        'packages'  => {
            'RecordMe'  => {
                'scalars'   => [ qw<scalar_state> ],
            },
        },
    } );
}

my $dummy;
my @dummy;
my %dummy;

my @calls;

# Generate some activity for the recorder to record.

# Access some constants.

$dummy = RecordMe::PI;
$dummy = RecordMe::ARR;

# Access and manipulate package variables.

$dummy = $RecordMe::scalar_state = 47; # Tests FETCH
$dummy = $RecordMe::scalar_state = "A string";

@RecordMe::array_state = (1, 2, 3);
exists( $RecordMe::array_state[0] );    # Tests EXISTS
exists( $RecordMe::array_state[17] );
$dummy = @RecordMe::array_state;        # Tests FETCHSIZE

for my $val (@RecordMe::array_state) {
    $dummy = $val;                      # Tests FETCH
};

@RecordMe::array_state = ('a', 'b', 'c');
@dummy = @RecordMe::array_state; # Tests FETCH and FETCHSIZE (?)

%RecordMe::hash_state = (
    'circle'    => 'sphere',
    'square'    => 'cube',
    'cube'      => 'hypercube',
);
for my $key ( keys %RecordMe::hash_state ) {    # Tests FIRSTKEY and NEXTKEY
    $dummy = $RecordMe::hash_state{$key};       # Tests FETCH
}
exists( $RecordMe::hash_state{'circle'} );      # Tests EXISTS
exists( $RecordMe::hash_state{'line'} );
scalar( %RecordMe::hash_state );                # Tests SCALAR

%RecordMe::hash_state = (
    'sky'   => 'grey',
    'water' => 'green',
    'grass' => 'yellow',
);       
%dummy = %RecordMe::hash_state; # Tests FIRSTKEY, NEXTKEY and FETCH (?)

# Call a few subroutines.

# Void context
RecordMe::pos_or_neg(0);
push( @calls, 'pos_or_neg' );
RecordMe::pos_or_neg(-17);
push( @calls, 'pos_or_neg' );
RecordMe::pos_or_neg(17);
push( @calls, 'pos_or_neg' );

# Scalar context
$dummy = RecordMe::pos_or_neg(5);
push( @calls, 'pos_or_neg' );
$dummy = RecordMe::pos_or_neg(0);
push( @calls, 'pos_or_neg' );
$dummy = RecordMe::pos_or_neg(-4);
push( @calls, 'pos_or_neg' );

# List context
@dummy = RecordMe::pos_or_neg(12);
push( @calls, 'pos_or_neg' );

# Tests exception handling
$dummy = RecordMe::throw(0);            # Doesn't die.
push( @calls, 'throw' );
eval { $dummy = RecordMe::throw(1); };  # Does.
push( @calls, 'throw' );

# Objects and Methods

my $obj = RecordMe->new();
push( @calls, 'new' );

$obj->put( 'hello', 'bill' );
push( @calls, 'put' );
$obj->put( 'bye', 'jane' );
push( @calls, 'put' );
$dummy = $obj->get('hello'); # Access data via method calls.
push( @calls, 'get' );
$dummy = $obj->get('bye');
push( @calls, 'get' );

my $obj2 = RecordMe->new();
push( @calls, 'new' );

$obj2->put( 'apple', 'red' );
push( @calls, 'put' );
$obj2->put( 'banana', 'yellow' );
push( @calls, 'put' );
$obj2->put( 'orange', 'orange' );
push( @calls, 'put' );
$obj2->put( undef, 'ugly' );
push( @calls, 'put' );

for my $key ( keys %{$obj2} ) { # Access data directly.
    $dummy =  $obj2->{$key};
}

# Inherited method
$dummy = RecordMe->mom('Does this return mom?');
push( @calls, 'mom' );
$dummy = RecordMe->grandma('Does this return grandma?'); 
push( @calls, 'grandma' );

# Watching a nested structure.

my $obj3 = RecordMe->new();
push( @calls, 'new' );
my $ref = {};

$obj3->put( 'nothing particularly exciting', 0 );
push( @calls, 'put' );
$obj3->put( 'a hash!', $ref );
push( @calls, 'put' );

$ref->{'Beer'} = 'Reel Big Fish'; #Song name => band name, just for fun :)
$ref->{'Spiderwebs'} = 'No Doubt';
$ref->{'Ojos Sexys'} = 'Laurel Aitken';
$ref->{'Gangsters'} = 'The Specials';

for my $key ( keys %{$ref} ) {
    $dummy = $ref->{$key};
}

$ref->{'Beer'} = 'Mustard Plug';
$dummy = $ref->{'Beer'};

# Watching a circular structure.

my $cur = my $front = [];
for my $i ( 1 .. 20 ) {
    $dummy = $cur->[0] = 2 * $i;
    $cur = $cur->[1] = [];
}

$cur->[0] = 'again!';
$cur->[1] = $front;

$obj3->put( 'a circular linked list', $front ); # We don't watch random lexical variables, so pass it.
push( @calls, 'put' );

# Access the elements.
$cur = $front;
while ( $cur->[1] != $front ) {
    $dummy = $cur->[0];
    $cur = $cur->[1];
}

# Write to disk

eval { Test::Mimic::Recorder::finish(); };
ok( ! $@, 'Recording complete, written to disk.' );
Test::More::BAIL_OUT("Unable to write to disk: $@") if $@;

# Read the recording in. Check the data.
chdir('.test_mimic_recorder_data');

open( my $fh, '<', 'additional_info.rec' ) or die "Unable to open file: $!";
my $records;
{
    local $/ = undef;
    $records = <$fh>;
}
close($fh) or die "Unable to close file: $!";

my ( $typeglobs, $extra, $order ) = @{ destringify($records) };

open( $fh, '<', 'history_from_recorder.rec' ) or die "Unable to open file: $!";
{
    local $/ = undef;
    $records = <$fh>;
}
close($fh) or die "Unable to close file: $!";
my $references = destringify($records);

# Check the prototype info.

is( $extra->{'RecordMe'}{'PROTOTYPES'}{'with_prototype'}, '$@', 'Prototype recorded' );

# Check the flattened class hierarchy

my $ancestry = $extra->{'RecordMe'}->{'ISA'};
for my $isa ( qw< RecordMe Dad Grandma Grandpa Mom > ) {
    ok( exists( $ancestry->{$isa} ), "Ancestor $isa found." );
}

ok( keys %{$ancestry} == 5, 'No spurious ancestors.' );

# Note: The following tests are limited and VERY brittle. This will be the case until unwrap and destringify
# are implemented for the generator.

# Check subroutine call order, ignoring arguments and event type

for my $call ( @{$order} ) {
    my ( $pack, undef, $name ) = @{$call};
    is( $pack, 'RecordMe', "Correct package name $pack." );
    is( $name, shift(@calls), "Correct subroutine name $name." );
}

# Check a reference

my $hash_history = $references->[12]->[1]->[0]; # 12 is simply the index of a particular reference
for my $key ( 'Beer', 'Gangsters', 'Ojos Sexys', 'Spiderwebs' ) {
    ok( exists( $hash_history->{$key} ), "$key Key found." );
}

ok( keys %{$hash_history} == 4, 'No spurious keys.' );

ok( @{ $hash_history->{'Beer'} } == 2, 'History length good for Beer.' );
is( $hash_history->{'Beer'}->[0]->[1], 'Reel Big Fish', 'First item from Beer history correct.' );
is( $hash_history->{'Beer'}->[1]->[1], 'Mustard Plug', 'Second item from Beer history correct.' );

for my $key ( 'Gangsters', 'Ojos Sexys', 'Spiderwebs' ) {
    ok( @{ $hash_history->{$key} } == 1, "History length good for $key." );
    is( $hash_history->{$key}->[0]->[1], $ref->{$key}, "Only item from $key history correct.");
}

# Check a glob
my $key = "\$TML_destringify_val = [\n                         202,\n                 ".
    "        [\n                           'ARRAY',\n                          ".
    " [\n                             [\n                               200,\n  ".
    "                             'RecordMe'\n                             ],\n".
    "                             [\n                               200,\n     ".
    "                          'Does this return grandma?'\n                  ".
    "           ]\n                           ]\n                         ]\n   ".
    "                    ];\n";
                                             # So very very very very very very very brittle... (The number of very's is the number
                                             # of times this code has bitten me in the ass. NEW CONCERN: Our unary
                                             # counting system is becoming problematic. Perhaps we can switch to
                                             # binary, e.g. extremely very would be 2, extremely very extremely would
                                             # be 5 and so on.)

my $table = $typeglobs;
for my $info ( [ 'RecordMe', 'Package' ], [ 'grandma', 'Symbol' ], [ 'CODE', 'Code' ], [ $key, 'Arg' ] ) {
    my ($key, $type) = @{$info};
    ok( exists( $table->{$key} ), "$type exists" );
    $table = $table->{$key};
}

ok( $table->[0]->[1]->[0] == Test::Mimic::Recorder::RETURN(), 'Correct behavior type.' );
is( $table->[0]->[1]->[1]->[1], 'grandma', 'Correct return' );
