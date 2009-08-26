use Test::Mimic::Generator;

my $gen = Test::Mimic::Generator->new();

$gen->load('.test_mimic_recorder_data');

$gen->write('.test_mimic_data');
