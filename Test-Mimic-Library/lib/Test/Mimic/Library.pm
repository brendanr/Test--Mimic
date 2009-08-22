package Test::Mimic::Library;

use 5.006001;
use strict;
use warnings;

use Test::Mimic::Library::MonitorScalar;
use Test::Mimic::Library::MonitorArray;
use Test::Mimic::Library::MonitorHash;
use Test::Mimic::Library::PlayScalar;
use Test::Mimic::Library::PlayArray;
use Test::Mimic::Library::PlayHash;

use Data::Dump::Streamer qw<:undump>;
use Scalar::Util qw<blessed refaddr reftype weaken readonly>;

require Exporter;

our @ISA = qw<Exporter>;

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
        ENCODE_TYPE
        DATA
        DATA_TYPE
        HISTORY
        CLASS
    ) ],
);

our @EXPORT_OK = (
    qw<
        encode
        decode
        monitor
        play
        monitor_aliases
        play_aliases
        gen_arg_key
        gen_arg_key_by
        stringify
        stringify_by
        destringify
        destringify_by
        init_records
        load_records
        get_references
        execute
    >,
    @{ $EXPORT_TAGS{'constants'} },
);

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

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

    # Event types. Should we deprecate this?
    CODE_E      => 500,
    SCALAR_E    => 501,
    ARRAY_E     => 502,
    HASH_E      => 503,

    # Encoded data fields, i.e. indices.
    ENCODE_TYPE => 0,
    DATA        => 1,

    # Reference table item fields, i.e. indices.
    DATA_TYPE   => 0,
    HISTORY     => 1,
    CLASS       => 2,

};

my $references; # A table containing recorded data for volatile references and objects. The index of a
                # given reference is simply the number of references
                # monitor saw before the reference under
                # consideration.
my $address_to_index;
my $is_alive;
my $index_to_reference;

sub init_records {
    $references = [];
    $address_to_index = {};
    $is_alive = {};
    $index_to_reference = {};
}

# Preloaded methods go here.

sub load_records {
    my ($file_name) = @_;

    init_records();
    
    open( my $fh, '<', $file_name ) or die "Could not open file: $!";

    my $recorded_data;
    {
        local $/;
        undef $/;
        $recorded_data = <$fh>;
    }

    my $ARRAY1;
    eval($recorded_data . '; 1;')
        or die "Unable to evaluate stored references in <$file_name>: $@";
    $references = $ARRAY1;

    close($fh) or die "Could not close file: $!";
}

sub get_references {
    return $references;
}

sub execute {
    my $behavior = shift(@_);

    # Find proper behavior for these arguments.
    my $key = gen_arg_key(@_);
    if ( ! exists( $behavior->{$key} ) ) {
        die 'No call recorded with corresponding arguments.';
    }
    my $context_to_result = $behavior->{$key};

    # Find proper behavior for this context.
    my $index;
    if (wantarray) {
        $index = LIST_CONTEXT;
    }
    elsif ( defined wantarray ) {
        $index = SCALAR_CONTEXT;
    }
    else {
        $index = VOID_CONTEXT;
    }
    my $results = $context_to_result->[$index];
    if ( ! defined( $results ) ) {
        die 'No call recorded in this context.';
    }

    # Obtain the results for this call.
    if ( @{$results} == 0 ) {
        die 'Call history exhausted.';
    }

    my ( $arg_signature, $stored_result ) = splice( @{$results}, 0, 2 );

    # Tie arguments making them behave as they were recorded behaving.
    play_aliases( \@_, $arg_signature );
    
    # Perform appropriately
    my ( $result_type, $result ) = @{$stored_result};
    if ( $result_type == EXCEPTION ) {
        die decode( $result );
    }
    elsif ( $result_type == RETURN ) {
        if (wantarray) {
            return @{ decode($result) };
        }
        elsif ( defined wantarray ) {
            return decode($result);
        }
        else {
            return;
        }
    }
    else {
        die "Bad result type <$result_type>.";
    }
}

# Accepts a single argument. Returns true iff the argument is a regular expression created by qr.
sub _is_pattern {
    return Data::Dump::Streamer::regex( $_[0] );
}

{
    my $translator = sub { return 'Will this key cause collisions?'; }; # ;)

    # Basic idea: Lightweight nesting. max depth 2, refs and objs identified by
    # type/name and index, unwatched/beyond max depth refs and objs simply by
    # type/name. Stringified.

    sub gen_arg_key_by {
        $translator = $_[0];
    }

    sub gen_arg_key {
        return &{$translator};
    }
}

{
    my $stringifier = sub { return scalar Data::Dump::Streamer::Dump( $_[0] )->Out(); };

    sub stringify_by {
        $stringifier = $_[0];
    }

    # Given an encoded element returns a string version. Should be suitable for use as a key in a hash as well as
    # being invertible with destringify. Subclass Test::Mimic::Recorder to stringify with your preferred library.
    #
    # TODO: Will need to use SortKeys from Data::Dump::Streamer if we start allowing unblessed hashes to be
    # encoded. This is necessary because they may be used in an argument list and these need to match exactly
    # from recording to playback. _encode currently will encode hashes, but the only places where we encode as
    # opposed to monitor are in argument lists and return lists (and only for one 'level'), so this is not at
    # present a problem.
    sub stringify {
        return &{$stringifier};
    }
}

{
    my $destringifier = sub { die "destringify not yet implemented." };

    sub destringify_by {
        $destringifier = $_[0];
    }

    sub destringify {
        return &{$destringifier};
    }
}


# aliases act like references, but look like simple scalars. Because of this we have to be particularly
# cautious where they could appear. Barring XS code and the sub{\@_} construction we only need to worry
# about subroutine arguments, i.e. $_[i].
#
# Accepts a reference to an array of aliases,
# e.g. @_ from another subroutine. It will monitor each alias that is not read-only and return a tuple
# consisting of the total number of aliases from the array reference as well as a hash reference that takes
# an index of a mutable element in the array to the result of monitor being called on a reference to said
# element.
sub monitor_aliases {
    my ( $aliases ) = @_;

    my $num_aliases = @{$aliases};
    my %mutable;
    for ( my $i = 0; $i < $num_aliases; $i++ ) {
        if ( ! readonly( $aliases->[$i] ) ) {
            $mutable{$i} = monitor( \$aliases->[$i] );
        }
    }
    return [ $num_aliases, \%mutable ];
}


# Accepts an array of aliases and the tuple returned by monitor_aliases.
# Attempts to match the aliases in the array reference with those in the tuple. If everything matches the
# mutable passed aliases will be tied to behave as those monitored earlier, otherwise dies. The array and
# the tuple representing the original array are said to match if the total number of elements are the same
# and the mutable elements are the same, i.e. appear at the same indices.
sub play_aliases {
    my ( $aliases, $coded_aliases ) = @_;
    my ( $orig_num_aliases, $mutable ) = @{$coded_aliases};

    # Apply a primitive signature check, list length.
    my $cur_num_aliases = @{$aliases};
    if ( $orig_num_aliases != $cur_num_aliases ) {
        die "Signatures do not match. Unable to play_aliases from <$coded_aliases> onto <$aliases>.";
    }

    # Consider each alias, tie the mutable aliases if everything matches, else die.
    for ( my $i = 0; $i < $cur_num_aliases; $i++ ) { 
        my $cur_read_only = readonly( $aliases->[$i] );
        my $orig_read_only = ! exists( $mutable->{$i} );

        if ( $cur_read_only && $orig_read_only ) {  # If they are both read-only they match.
            next;                                   # We shouldn't try to tie a read-only variable. :)
        }
        elsif ( ! $cur_read_only && ! $orig_read_only ) { # If they are both mutable...
            my $index = $mutable->{$i}->[DATA]; # See monitor.

            if ( defined( $index_to_reference->{$index} ) ) { # If we have already seen this value.
                next;
            }

            #TODO: Assuming we maintain address_to_index and is_alive during playback too we can
            #      check to see if $address_to_index{ refaddr( $index_to_reference{$index} ) } == $index.
            #      If it doesn't we know that there is a problem. <---- that like something Or.

            my ( $type, $history, $old_class ) = @{ $references->[$index] };
            tie( $aliases->[$i], 'Test::Mimic::Library::PlayScalar', $history );
            $index_to_reference->{$index} = \( $aliases->[$i] );
            weaken( $index_to_reference->{$index} ); # Don't prevent the val from being gced.

            #NOTE: We need not bless the alias here. Either we produced it earlier, blessed it then and hit
            #      next above or the alias was produced externally and if blessed at all was blessed
            #      elsewhere.

            my $address = refaddr( \( $aliases->[$i] ) ); 
            $address_to_index->{$address} = $index;
            $is_alive->{$address} = \( $aliases->[$i] );
            weaken( $is_alive->{$address} );

        }
        else {
            die "Mutable/immutable mismatch. Unable to play_aliases from <$coded_aliases> onto "
                . "<$aliases>.";
        }
    }
}

sub _get_type {
    my ($val) = @_;

    if ( _is_pattern($val) ) {
        return 'REG_EXP';
    }
    else {
        my $type = reftype($val);
        if ( $type eq 'REF' || $type eq 'LVALUE' || $type eq 'VSTRING' ) {
            return 'SCALAR';
        }
        else {
            return $type;
        }
    }
}

{
    # Each of these helper subroutines takes ( $val, $type ).
    my $scalar_action = sub {
        my $history = [];
        tie( ${ $_[0] }, 'Test::Mimic::Library::MonitorScalar', $history, $_[0] );
        return [ 'SCALAR', $history ];
    };
    my $simple_action = sub { return [ $_[1], $_[0] ]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => $scalar_action,
        'REF'       => $scalar_action,
        'LVALUE'    => $scalar_action,
        'VSTRING'   => $scalar_action,
        'ARRAY'     => sub {
            my $history = [];
            tie ( @{ $_[0] }, 'Test::Mimic::Library::MonitorArray', $history, $_[0] );
            return [ 'ARRAY', $history ];
        },
        'HASH'      => sub {
            my $history = [];
            tie ( %{ $_[0] }, 'Test::Mimic::Library::MonitorHash', $history, $_[0] );
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
    # 'object' behind the reference) can not change from being blessed to being unblessed anywhere that monitor
    # will notice. Purely internal modifications, i.e. those occurring in a wrapped subroutine, are okay.
    # Additionally, modifications occurring prior to the reference being monitored are okay. Also, it should be
    # noted that references blessed into a package that is not being recorded will have their state recorded
    # properly (including object info), but that object method calls on that reference will still not be
    # recorded.
    sub monitor {
        my ( $val ) = @_;

        my $type = reftype($val);
        if ( ! $type ) { # If this is not a reference...
            return [ STABLE, $val ];
        }
        else {
            my $address = refaddr($val);
            my $index;

            if ( defined( $is_alive->{$address} ) ) {      # If we are watching this reference...

                # NOTE: We are using defined as opposed to exists because a given address can be used by multiple
                # references over the entire execution of the program. See the comment on weaken below.

                $index = $address_to_index->{$address};
            }
            else {
                # Note that we are watching the reference.
                $is_alive->{$address} = $val;
                weaken( $is_alive->{$address} );   # This reference will be automatically set to undef when $$val is
                                                # garbage collected.

                if ( _is_pattern($val) ) { # reftype doesn't recognize patterns, so set $type manually. 
                    $type = 'REG_EXP';
                }

                # Create a representation of the reference depending on its type.
                # Monitors recursively as necessary.
                my $reference;
                if ( exists( $type_to_action{$type} ) ) {
                    $reference = &{ $type_to_action{$type} }( $val, $type );
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
    # Each of these helper subroutines takes ( $val, $at_level, $type ).
    my $scalar_action = sub { return [ 'SCALAR', encode( ${ $_[0] }, $_[1] ) ]; };
    my $simple_action = sub { return [ $_[2], $_[0] ]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => $scalar_action,
        'REF'       => $scalar_action,
        'LVALUE'    => $scalar_action,
        'VSTRING'   => $scalar_action,
        'ARRAY'     => sub {
            my @temp = map( { encode( $_, $_[1] ) } @{ $_[0] } );
            return [ 'ARRAY', \@temp ];
        },
        'HASH'      => sub {
            my %temp;
            @temp{ keys %{ $_[0] } } = map( { encode( $_[0]->{$_}, $_[1] ) } keys %{ $_[0] } );
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
    # Currently scalars et al., arrays, hashes, qr objects, code references are handled well.
    # Filehandles are not being tied, ideally they would be, but the filehandle tying mechanism is
    # not complete.
    # Formats are in a similar position, but they probably shouldn't ever be redefined. (Check this.)
    # Because of this that may not really be a problem.
    # The entries in globs can not be tied. A special glob tie could potentially remedy this, but
    # this does not currently exist.
    #
    # TODO: Handle circular references, also save space on DAGs.
    # Idea: Scan through structure. Record all references in a big hash. If we see duplicates note them.
    # The duplicates will exist as a special structure.
    #
    # [ CIRCULAR_NESTED, <dup_table>, [ ARRAY, blah...
    # We have one additional type:
    # [ DUP, <index> ]
    sub encode {
        my ( $val, $at_level ) = @_;

        if ( $at_level == 0 ) { # If we have reached the volatile layer...
            return monitor( $val );
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
            my $coded = &{ $type_to_action{$type} }( $val, $at_level, $type );
            return [ NESTED, $coded ];
        }
        else {
            die "Unknown reference type <$type> from <$val>. Unable to encode.";
        }
    }
}

{
    # Each of these helper subroutines takes ( $val ).
    my $simple_action = sub { return $_[0]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => sub {
            my $temp = decode( $_[0] );
            return \$temp;
        },
        'ARRAY'     => sub {
            my @temp = map( { decode( $_ ) } @{ $_[0] } );
            return \@temp;
        },
        'HASH'      => sub {
            my %temp;
            @temp{ keys %{ $_[0] } } = map( { decode( $_[0]->[$_] ) } keys %{ $_[0] } );
            return \%temp;
        },
        'GLOB'      => $simple_action,
        'IO'        => $simple_action,
        'FORMAT'    => $simple_action,
        'CODE'      => $simple_action,
    );

    sub decode {
        my ( $coded_val ) = @_;
        my ( $code_type, $data ) = @{$coded_val};

        if ( $code_type == STABLE ) {
            return $data;
        }
        elsif ( $code_type == NESTED ) {
            my ( $ref_type, $val ) = @{$data};
        
            if ( exists( $type_to_action{$ref_type} ) ) {
                return &{ $type_to_action{$ref_type} }( $val );
            }
            else {
                die "Invalid reference type <$ref_type> from <$data> with value <$val>. Unable to decode.";
            }
        }
        elsif ( $code_type == VOLATILE ) {
            return play( $coded_val ); 
        }
        else {
            die "Invalid code type <$code_type> from <$coded_val> with data <$data>. Unable to decode.";
        }
    }
}

{
    # Each of these helper subroutines takes ( $history ).
    # This will be a single reference, i.e. not a true history, for types we do not tie.
    my $simple_action = sub { return $_[0]; };
    my %type_to_action = (
        'REG_EXP'   => $simple_action,
        'SCALAR'    => sub {
            my $temp;
            tie( $temp, 'Test::Mimic::Library::PlayScalar', $_[0] );
            return \$temp;
        },
        'ARRAY'     => sub {
            my @temp;
            tie( @temp, 'Test::Mimic::Library::PlayArray', $_[0] );
            return \@temp;
        },
        'HASH'      => sub {
            my %temp;
            tie( %temp, 'Test::Mimic::Library::PlayHash', $_[0] );
            return \%temp;
        },
        'GLOB'      => $simple_action,
        'IO'        => $simple_action,
        'FORMAT'    => $simple_action,
        'CODE'      => $simple_action,
    );

    sub play {
        my ( $coded_val ) = @_;
 
        my ( $type, $data ) = @{$coded_val};
        if ( $type == STABLE ) {
            return $data;
        }
        elsif ( $type == VOLATILE ) {
            if ( defined( $index_to_reference->{$data} ) ) {    # We are using defined because the weak
                                                                # references used in the hash will be set to
                                                                # undef upon the destruction of the
                                                                # corresponding values.
                return $index_to_reference->{$data};
            }
            else {
                my ( $type, $history, $class_name ) = @{ $references->[$data] };
                
                my $reference;
                if ( exists( $type_to_action{$type} ) ) {
                    $reference = &{ $type_to_action{$type} }( $history );
                }
                else {
                    die "Unknown reference type <$type> at index <$data>. Unable to play.";
                }

                # If this reference is supposed to point at an object, bless it.
                # This will take place even if we didn't record the class. This may be a feature or a bug.
                if ( defined($class_name) ) {
                    bless( $reference, $class_name );
                }

                # Note the creation of this reference, so we don't recreate it and are aware of what recorded
                # reference it corresponds to.
                my $address = refaddr($reference); 
                $address_to_index->{$address} = $data;
                $is_alive->{$address} = $reference;
                weaken( $is_alive->{$address} );
                $index_to_reference->{$data} = $reference;
                weaken( $index_to_reference->{$data} ); # But don't prevent it from being gced. If we
                                                        # need to we can recreate it easily. ( Although the
                                                        # address may well be different. )

                return $reference;            
            }
        }
        else {
            die "Unrecognized type <$type>. Unable to play.";
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
