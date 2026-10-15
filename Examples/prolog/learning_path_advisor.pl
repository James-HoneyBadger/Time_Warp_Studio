/*
 * TIME WARP STUDIO LEARNING PATH ADVISOR
 * Facts, prerequisite inference, recursive paths, and recommendations.
 */

/* Available Studio topics and their learning tracks */
topic(expressions, basic).
topic(variables, basic).
topic(conditionals, basic).
topic(loops, basic).
topic(procedures, basic).
topic(lists, python).
topic(functions, python).
topic(turtle_art, logo).
topic(data_queries, prolog).
topic(stack_words, forth).
topic(memory_tape, brainfuck).

/* Sample learner record */
completed(expressions).
completed(variables).
interest(learner, graphics).
interest(learner, data).

/* Prerequisite graph */
requires(conditionals, variables).
requires(loops, conditionals).
requires(procedures, loops).
requires(lists, variables).
requires(functions, loops).
requires(turtle_art, procedures).
requires(data_queries, procedures).
requires(stack_words, loops).
requires(memory_tape, variables).

/* A topic is unlocked when its prerequisite is complete. */
unlocked(Topic) :-
    topic(Topic, _),
    requires(Topic, Prerequisite),
    completed(Prerequisite).

/* Follow prerequisite links recursively to build a learning route. */
path(Start, Start).
path(Start, Goal) :-
    requires(Goal, Previous),
    path(Start, Previous).

/* Recommend tracks that match the learner's recorded interests. */
recommended_track(logo) :- interest(learner, graphics).
recommended_track(prolog) :- interest(learner, data).

/* Printable report */
:- write('============================================================'), nl.
:- write('  TIME WARP STUDIO | LEARNING PATH ADVISOR'), nl.
:- write('  Prerequisites | Inference | Personalized next steps'), nl.
:- write('============================================================'), nl, nl.
:- write('Completed foundations: expressions and variables'), nl.
:- findall(Topic, unlocked(Topic), Topics),
   write('Unlocked topics: '), write(Topics), nl, nl.
:- findall(Track, recommended_track(Track), Tracks),
   write('Recommended language tracks: '), write(Tracks), nl, nl.
:- write('Prerequisite route: variables -> loops -> procedures -> turtle_art'), nl.
:- write('Use the lesson dock to explore any unlocked topic.'), nl.
