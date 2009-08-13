package Test::Mimic::Library;

use 5.006001;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use Test::Mimic::Library ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = (
    'constants' => [ qw(
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
    ) ],
);

our @EXPORT_OK = (
    qw<
        encode
        encode_aliases
        decode
        monitor
        play
    >,
    @{ $EXPORT_TAGS{'constants'} },
);

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

# Preloaded methods go here.

use Scalar::Util qw<blessed refaddr reftype weaken readonly>;

use constant {
    # Array indices for the three contexts
    SCALAR_CONTEXT  => 0,
    LIST_CONTEXT    => 1,
    VOID_CONTEXT    => 2,

    # Description of encoded data
    STABLE      => 200,
    VOLATILE    => 201,
    NESTED      => 202,

    # The two types of supported behavior
    RETURN      => 300,
    EXCEPTION   => 301,

    # Convenience values
    ARBITRARY   => 400, # For merely creating hash entries

    # Event types
    CODE_E      => 500,
    SCALAR_E    => 501,
    ARRAY_E     => 502,
    HASH_E      => 503,
};

# Accepts a single argument. Returns true iff the argument is a regular expression created by qr.
sub _is_pattern {
    return Data::Dump::Streamer::regex( $_[0] );
}

# aliases act like references, but look like simple scalars. Because of this we have to be particularly
# cautious where they could appear. Barring XS code and the sub{\@_} construction we only need to worry
# about subroutine arguments, i.e. $_[i].
#
# _encode_aliases accepts a list of aliases and returns an encoded form of the list. This will be suitable
# for stringification. All aliases that are not read only will be _monitored. We should not ever need to
# decode the result becuase we are only using it to create a hash key.
sub monitor_arguments {
    my ( $records, $aliases ) = @_;

    my $num_args = @{$aliases};
    my %mutable;
    for ( my $i = 0, $i < $num_args, $i++ ) {
        if ( ! readonly( $aliases->[$i] ) ) {
            $mutable{$i} = monitor( $records, \$aliases->[$i] ) );
        }
    }
    return [ NESTED, [ 'ARRAY', \@coded ] ];
}

sub decode_aliases {
    my ( $records, $coded_aliases ) = @_;

    
    if ( 
}

{
    # Each of these helper subroutines takes ( $records, $val, $type ).
    my $scalar_action = sub {
        my $history = [];
        tie( ${ $_[1] }, 'Test::Mimic::Library::MonitorScalar', $_[0], $history, ${ $_[1] } );
        return [ 'SCALAR', $history ];
    };
    my $simple_action = sub { return [ $_[2], $_[1] ]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => $scalar_action,
        'REF'       => $scalar_action,
        'LVALUE'    => $scalar_action,
        'VSTRING'   => $scalar_action,
        'ARRAY'     => sub {
            my $history = [];
            tie ( @{ $_[1] }, 'Test::Mimic::Library::MonitorArray', $_[0], $history, $_[1] );
            return [ 'ARRAY', $history ];
        },
        'HASH'      => sub {
            my $history = [];
            tie ( %{ $_[1] }, 'Test::Mimic::Library::MonitorHash', $_[0], $history, $_[1] );
            return [ 'HASH', $history ];
        },
        'GLOB'      => $simple_action,
        'IO'        => $simple_action,
        'FORMAT'    => $simple_action,
        'CODE'      => $simple_action,
    );

    # Monitor, i.e. tie the value and record its state, if possible (recursively as needed), otherwise merely
    # encapsulate the value as well as possible. In the second case proper storage and retrivial of the data
    # becomes the responsibility of Test::Mimic::Recorder::stringify.
    #
    # Objects are handled, but to a limited extent. The main restriction is that a reference (or rather the
    # 'object' behind the reference) can not change from being blessed to being unblessed anywhere that _monitor
    # will notice. Purely internal modifications, i.e. those occurring in a wrapped subroutine, are okay.
    # Additionally, modifications occurring prior to the reference being _monitored are okay. Also, it should be
    # noted that references blessed into a package that is not being recorded will have their state recorded
    # properly (including object info), but that object method calls on that reference will still not be
    # recorded.
    sub monitor {
        my ( $records, $val ) = @_;

        my $type = reftype($val);
        if ( ! $type ) { # If this is not a reference...
            return [ STABLE, $val ];
        }
        else {
            my ( $references, $address_to_index, $alive ) = @{$records};
            my $address = refaddr($val);
            my $index;

            if ( defined( $alive->{$address} ) ) {      # If we are watching this reference...

                # NOTE: We are using defined as opposed to exists because a given address can be used by multiple
                # references over the entire execution of the program. See the comment on weaken below.

                $index = $address_to_index->{$address};
            }
            else {
                # Note that we are watching the reference.
                $alive->{$address} = $val;
                weaken( $alive->{$address} );   # This reference will be automatically set to undef when $$val is
                                                # garbage collected.

                if ( _is_pattern($val) ) { # reftype doesn't recognize patterns, so set $type manually. 
                    $type = 'REG_EXP';
                }

                # Create a representation of the reference depending on its type.
                # Monitors recursively as necessary.
                my $reference;
                if ( exists( $type_to_action{$type} ) ) {
                    $reference = &{ $type_to_action{$type} }( $records, $val, $type );
                }
                else {
                    die "Unknown reference type <$type> from <$val>. Unable to monitor.";
                }
                $reference->[2] = blessed($val); # Mark this as either an object or a plain reference.

                # Store the representation of the reference into the references table.
                push( @{$references}, $reference );
                $index = $address_to_index->{$address} = $#{$references};
            }
            return [ VOLATILE, $index ];
        }
    }
}

{
    # Each of these helper subroutines takes ( $records, $val, $at_level, $type ).
    my $scalar_action = sub { return [ 'SCALAR', _encode( $_[0], ${ $_[1] }, $_[2] ) ]; };
    my $simple_action = sub { return [ $_[3], $_[1] ]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => $scalar_action,
        'REF'       => $scalar_action,
        'LVALUE'    => $scalar_action,
        'VSTRING'   => $scalar_action,
        'ARRAY'     => sub {
            my @temp = map( { _encode( $_[0], $_, $_[2] ) } @{ $_[1] } );
            return [ 'ARRAY', \@temp ];
        },
        'HASH'      => sub {
            my %temp;
            @temp{ keys %{ $_[1] } } = map( { _encode( $_[0], $_[1]->{$_}, $_[2] ) } keys %{ $_[1] } );
            return [ 'HASH', \%temp];
        },
        'GLOB'      => $simple_action,
        'IO'        => $simple_action,
        'FORMAT'    => $simple_action,
        'CODE'      => $simple_action,
    );

    # Performs an expansion wrap on the passed value until the given level then watches every component below.
    # Returns a structure analogous to the original except that each component is recursively wrapped. This should
    # only be used on static data. If circular references exist above the watch level or into the wrap level the
    # behavior is undefined.
    #
    # For example if _watch was passed an array it would perhaps return [ VOLATILE, 453 ].
    # _wrap_then_watch would return [ NESTED, [ ARRAY, [ [ STABLE, 'foo' ], [ STABLE, 'bar' ] ] ] ]
    #
    # This is useful when the data currently in the array is important, but the array itself has no special
    # significance.
    #
    # TODO: Handle circular references, also save space on DAGs.
    # Idea: Scan through structure. Record all references in a big hash. If we see duplicates note them.
    # The duplicates will exist as a special structure.
    #
    # [ CIRCULAR_NESTED, <dup_table>, [ ARRAY, blah...
    # We have one additional type:
    # [ DUP, <index> ]
    sub encode {
        my ( $records, $val, $at_level ) = @_;

        if ( $at_level == 0 ) { # If we have reached the volatile layer...
            return _monitor( $records, $val );
        }
        else {
            $at_level--;
        }

        my $type = reftype($val);
        if ( ! $type ) { # If the value is not a reference...
            return [ STABLE, $val ];
        }
        elsif ( exists( $type_to_action{$type} ) ) {
            if ( _is_pattern($val) ) { # reftype doesn't recognize patterns, so set $type manually.
                $type = 'REG_EXP';
            }
            my $coded = &{ $type_to_action{$type} }( $records, $val, $at_level, $type );
            return [ NESTED, $coded ];
        }
        else {
            die "Unknown reference type <$type> from <$val>. Unable to encode.";
        }
    }
}

{
    # Each of these helper subroutines takes ( $records, $val ).
    my $simple_action = sub { return $_[1]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => sub {
            my $temp = decode( $_[0], $_[1] );
            return \$temp;
        },
        'ARRAY'     => sub {
            my @temp = map( { decode( $_[0], $_ ) } @{ $_[1] } );
            return \@temp;
        },
        'HASH'      => sub {
            my %temp;
            @temp{ keys %{ $_[1] } } = map( { decode( $_[0], $_[1]->[$_] ) } keys %{ $_[1] } );
            return \%temp;
        },
        'GLOB'      => $simple_action,
        'IO'        => $simple_action,
        'FORMAT'    => $simple_action,
        'CODE'      => $simple_action,
    );

    sub decode {
        my ( $records, $coded_val ) = @_;
        my ( $code_type, $data ) = @{$coded_val};

        if ( $code_type == STABLE ) {
            return $data;
        }
        elsif ( $code_type == NESTED ) {
            my ( $ref_type, $val ) = @{$data};
        
            if ( exists( $type_to_action{$ref_type} ) ) {
                return &{ $type_to_action{$ref_type} }( $records, $val );
            }
            else {
                die "Invalid reference type <$ref_type> from <$data> with value <$val>. Unable to decode.";
            }
        }
        elsif ( $code_type == VOLATILE ) {
            return _play( $records, $data ); 
        }
        else {
            die "Invalid code type <$code_type> from <$coded_val> with data <$data>. Unable to decode.";
        }
    }
}

{
    my $simple_action = sub { return $_[1]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => sub {
            my $temp;
            tie( $temp, 'Test::Mimic::Library::PlayScalar', $_[0], $_[2] );
            return \$temp;
        },
        'ARRAY'     => sub {
            my @temp;
            tie( @temp, 'Test::Mimic::Library::PlayArray', $_[0], $_[2] );
            return \@temp;
        },
        'HASH'      => sub {
            my %temp;
            tie( %temp, 'Test::Mimic::Library::PlayHash', $_[0], $_[2] );
            return \%temp;
        },
        'GLOB'      => $simple_action,
        'IO'        => $simple_action,
        'FORMAT'    => $simple_action,
        'CODE'      => $simple_action,
    );

    sub play {
        my ( $records, $index ) = @_;
        my ( $references, $index_to_references ) = @{$records};

        if ( exists( $index_to_references->[$index] ) ) {
            return $index_to_references->[$index];
        }
        else {
            my ( $type, $history, $class_name ) = @{ $references->[$index] };
            
            my $reference;
            if ( exists( $type_to_action{$type} ) ) {
                $reference = &{ $type_to_action{$type} }( $records, $type, $history );;
            }
            else {
                die "Unknown reference type <$type> at index <$index>. Unable to play.";
            }

            # If this reference is supposed to point at an object, bless it.
            # This will take place even if we didn't record the class. This may be a feature or a bug.
            if ( defined($class_name) ) {
                bless( $reference, $class_name );
            }

            # Note the creation of this reference, so we don't recreate it.
            $index_to_references->{$index} = $reference;

            return $reference;            
        }
    }
}


1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Test::Mimic::Library - Perl extension for blah blah blah

=head1 SYNOPSIS

  use Test::Mimic::Library;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Test::Mimic::Library, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Brendan Roof, E<lt>broof@E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Brendan Roof

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
