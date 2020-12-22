grammar Day21;

food: ingredients '(contains' allergens ')';
ingredients: WORD+;
allergens: WORD+;
WORD: [a-z]+;
WHITESPACE: [ \r\n] -> skip;
