durak
=====

AI for the card game of Durak (https://en.wikipedia.org/wiki/Durak)

Compiling
---------

Doesn't use any non-core libraries, so `cabal configure` and `cabal run` should work.

Playing
-------

The list of actions you can perform is displayed at every step in the game.

AI
--

The AI works in two game stages: early-game and endgame.

In early-game, the opponent's cards cannot be reliably inferred, so the AI tries to select actions based on maximizing the future value of its hand: if I perform this action and take cards on the table/from the deck, what will my hand value will be? The deck value is calculated as the average value of all cards in the hand, with a penalty for hands of size greater than 6. A card's value is its defense value: what fraction of cards (expected value) in the opponent's hand can it beat? This means that the AI's logic is changed as it sees cards played by the opponent. The estimate is improved by considering the cards that we saw the opponent take from the desk after a failed defense.

A problem with this approach is that if we have great cards in our hand, we'll have to attack with them eventually, thus giving the opponent free cards.

The aim of the early-game strategy is to leave us with a good hand for the endgame, where both our and the opponent's hand is known. In the endgame, the AI uses a minimax search with some caching and canonical forms (several ways we can get to the same game state, but it might be represented differently) to try and win the game. A problem the minimax sometimes encounters is the combinatorial explosion: say, if we have to beat 3 cards and we have 3 cards, each of which can beat them, there are 6 ways to perform this and the AI searches through all of them.
