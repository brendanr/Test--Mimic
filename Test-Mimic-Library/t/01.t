use Test::More 'no_plan';

use Data::Dump::Streamer qw< Dump >;

BEGIN {
    use_ok( 'Test::Mimic::Library', qw<

        SCALAR_CONTEXT
        LIST_CONTEXT
        VOID_CONTEXT
        STABLE
        VOLATILE
        NESTED
        RETURN
        EXCEPTION
        ARBITRARY
        CODE_E
        SCALAR_E
        ARRAY_E
        HASH_E
        ENCODE_TYPE
        DATA
        DATA_TYPE
        HISTORY
        CLASS
 
        encode
        decode
        monitor
        play
        monitor_args
        monitor_args_by
        play_args
        play_args_by
        gen_arg_key
        gen_arg_key_by
        stringify
        stringify_by
        destringify
        destringify_by
        init_records
        load_records
        write_records
        get_references
        load_preferences
        execute
        descend
    > );
}

init_records();

is_deeply( get_references(), [], 'a get_references() call after init_records() suggests that initialization'
    . ' occurred properly.' );

my $light_encode_io_pairs = [
    [
        4,
        [ STABLE, 4 ]
    ],
    [
        'hello',
        [ STABLE, 'hello' ]
    ],
    [
        [ 'a', 2, 'b' ],
        [ NESTED, [ ARRAY, [ [ STABLE, 'a' ], [ STABLE, 2 ], [ STABLE, 'b' ] ] ] ]
    ],
    [
        [ [ [ 'foo' ] ] ],
        [ NESTED, [ ARRAY, [ [ NESTED, [ ARRAY , [ [ NESTED, [ ARRAY  ] ] ] ] ] ] ] ]
    ],
    [
        [ [ 'foo' ] ],
        [ NESTED, [ ARRAY , [ [ NESTED, [ ARRAY, [ [ STABLE, 'foo' ] ]  ] ] ] ] ]
    ],
    [
        sub {},
        [ NESTED, [ CODE ] ]
    ],
    [
        { 'x' => 'y', 'a' => 'b' },
        [ NESTED, [ HASH, { 'x' => [ STABLE, 'y' ], 'a' => [ STABLE, 'b' ] } ] ]
    ],
    [
        \\\'a',
        [ NESTED, [ SCALAR, [ NESTED, [ SCALAR, [ NESTED, [ SCALAR ] ] ] ] ] ]
    ],
    [
        \\'a',
        [ NESTED, [ SCALAR, [ NESTED, [ SCALAR, [ STABLE, 'a' ] ] ] ] ]
    ],
];

my $i = 1;
for my $pair ( @{$light_encode_io_pairs} ) {
    my ( $input, $output ) = @{$pair};

    is_deeply( Test::Mimic::Library::_light_encode( $input, 2 ), $output , "basic _light_encode test number $i" );
    $i++;
}

