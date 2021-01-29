grammar Day12;

initial_state: 'initial state: ' pots EOL;
pots: pot+;
pot: '.' | '#';

transformations: transformation+;
transformation: pot pot pot pot pot '=>' newPot EOL;
newPot: '.' | '#';

WHITESPACE: [\r\n ] -> skip;
