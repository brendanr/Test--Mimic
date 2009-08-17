
use Test::Mimic::Generator;

my $gen = Test::Mimic::Generator->new();

$gen->load('save_to');

$gen->write('fake_lib');
