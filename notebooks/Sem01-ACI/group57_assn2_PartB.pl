% ---------------------------------------------------------------
:- dynamic(patient_has/2).      % patient_has(Patient, Symptom)  -- positives
:- dynamic(patient_not/2).     % patient_not(Patient, Symptom)  -- negatives
:- dynamic(asked/2).           % asked(Patient, Symptom)        -- asked questions

% ------------------------
% Disease -> symptom facts
% (Define Facts)
% ------------------------
disease_symptom(common_cold, runny_nose).
disease_symptom(common_cold, sneezing).
disease_symptom(common_cold, sore_throat).
disease_symptom(common_cold, mild_fever).

disease_symptom(flu, high_fever).
disease_symptom(flu, body_aches).
disease_symptom(flu, fatigue).
disease_symptom(flu, cough).
disease_symptom(flu, sore_throat).

disease_symptom(allergy, runny_nose).
disease_symptom(allergy, sneezing).
disease_symptom(allergy, itchy_eyes).
disease_symptom(allergy, skin_rash).

disease_symptom(strep_throat, sore_throat).
disease_symptom(strep_throat, high_fever).
disease_symptom(strep_throat, swollen_lymph_nodes).
disease_symptom(strep_throat, white_spots_on_tonsils).

% helper to list known diseases
disease(Name) :- disease_symptom(Name, _).

% ------------------------
% Basic bookkeeping
% ------------------------
clear_patient(Name) :-
    retractall(patient_has(Name, _)),
    retractall(patient_not(Name, _)),
    retractall(asked(Name, _)).

mark_positive(P, S) :- ( patient_has(P, S) -> true ; assertz(patient_has(P, S)) ).
mark_negative(P, S) :- ( patient_not(P, S) -> true ; assertz(patient_not(P, S)) ).
mark_asked(P, S)    :- ( asked(P, S) -> true ; assertz(asked(P, S)) ).

% ------------------------
% Interaction helpers
% ------------------------
ask_yesno(Patient, Symptom) :-
    format('Does ~w have ~w? (yes. / no.)~n', [Patient, Symptom]),
    read(Resp),
    normalize_response(Resp, Norm),
    ( Norm = yes ->
        mark_positive(Patient, Symptom),
        mark_asked(Patient, Symptom)
    ; Norm = no ->
        mark_negative(Patient, Symptom),
        mark_asked(Patient, Symptom)
    ; writeln('Please answer with yes. or no. (include the final period)'),
      ask_yesno(Patient, Symptom)
    ).

normalize_response(yes, yes).
normalize_response(y, yes).
normalize_response(no, no).
normalize_response(n, no).

ask_fever(Patient, Level) :-
    format('Does ~w have fever? (none. / mild. / high.)~n', [Patient]),
    read(A),
    ( A == mild ->
        Level = mild,
        mark_positive(Patient, mild_fever),
        mark_asked(Patient, mild_fever)
    ; A == high ->
        Level = high,
        mark_positive(Patient, high_fever),
        mark_asked(Patient, high_fever)
    ; A == none -> Level = none
    ; writeln('Please answer none. or mild. or high. (include final period)'),
      ask_fever(Patient, Level)
    ).

% ------------------------
% Candidate diseases based on answers so far
% - We only eliminate diseases contradicted by a known negative symptom
% ------------------------
candidates(Patient, Candidates) :-
    setof(D, disease(D), All),
    include(not_contradicted_by_neg(Patient), All, Candidates).

not_contradicted_by_neg(Patient, Disease) :-
    \+ ( disease_symptom(Disease, Sym), patient_not(Patient, Sym) ).

% disease fully confirmed if all its symptoms are marked present
confirmed_diseases(Patient, Confirmed) :-
    setof(D,
          ( disease(D),
            findall(S, disease_symptom(D,S), Sys),
            all_present(Patient, Sys)
          ),
          Confirmed), !.
confirmed_diseases(_, []).  % none confirmed

all_present(_, []).
all_present(Patient, [H|T]) :-
    patient_has(Patient, H),
    all_present(Patient, T).

% -----------------------------------------------------
% Entropy and Information Gain calculation
% -----------------------------------------------------

log2(X, R) :- X > 0, R is log(X) / log(2).
log2(0, 0) :- !.

% entropy for a set of equally-weighted candidates
entropy(List, H) :-
    ( List = [] -> H = 0
    ; length(List, N),
      ( N =:= 0 -> H = 0
      ; P is 1.0 / N,
        log2(P, Lp),
        H is - N * P * Lp
      )
    ).

% for a symptom, compute expected entropy after asking it:
% partition candidate diseases into "have symptom" and "do not have symptom"
expected_entropy_after(Patient, Symptom, ExpectedH) :-
    candidates(Patient, Cands),
    ( Cands = [] -> ExpectedH = 0
    ; partition(has_symptom(Symptom), Cands, Yes, No),
      length(Cands, Tot),
      length(Yes, Ny), length(No, Nn),
      ( Tot =:= 0 -> ExpectedH = 0
      ; Py is Ny / Tot, Pn is Nn / Tot,
        entropy(Yes, Hy), entropy(No, Hn),
        ExpectedH is Py * Hy + Pn * Hn
      )
    ).

has_symptom(Symptom, Disease) :- disease_symptom(Disease, Symptom).

info_gain(Patient, Symptom, Gain) :-
    candidates(Patient, Cands),
    entropy(Cands, Hcur),
    expected_entropy_after(Patient, Symptom, Hafter),
    Gain is Hcur - Hafter.

% ------------------------
% Which symptoms we can still ask
% ------------------------
all_symptoms(Syms) :- setof(S, D^disease_symptom(D,S), Syms), !.
all_symptoms([]).

remaining_symptoms(Patient, Remaining) :-
    all_symptoms(All),
    exclude(already_asked(Patient), All, Remaining).

already_asked(Patient, S) :- asked(Patient, S).

% choose symptom with highest information gain
best_symptom(Patient, Best) :-
    remaining_symptoms(Patient, Rem),
    Rem \= [],
    findall(G-S, ( member(S, Rem), info_gain(Patient, S, G) ), Pairs),
    sort(0, @>=, Pairs, Sorted),
    Sorted = [_G-Best | _].

% ------------------------
% If questioning ends without a conclusive single confirmed disease,
% pick the disease that matches the most positive signs.
% ------------------------
best_match(Patient, Best) :-
    setof(D, disease(D), AllDs),
    findall(Score-D,
            ( member(D, AllDs),
              findall(S, disease_symptom(D,S), Syms),
              count_present(Patient, Syms, Score)
            ),
            Pairs),
    keysort(Pairs, Asc),
    reverse(Asc, [MaxScore-Best | _]),
    ( MaxScore =< 0 -> Best = none ; true ).

count_present(_, [], 0).
count_present(Patient, [H|T], Count) :-
    count_present(Patient, T, C0),
    ( patient_has(Patient, H) -> Count is C0 + 1 ; Count is C0 ).

% ------------------------
% Main interactive loop
% ------------------------
diagnose_loop(Patient, Final) :-
    candidates(Patient, Cands),
    ( confirmed_diseases(Patient, [D|_]) ->
        Final = D,
        format('Diagnosis: ~w (all required symptoms matched).~n', [D])
    ; Cands = [Only] ->
        Final = Only,
        format('Diagnosis: ~w (only remaining candidate).~n', [Only])
    ; ( best_symptom(Patient, Sym) ->
          ask_yesno(Patient, Sym),
          diagnose_loop(Patient, Final)
      ; % no symptoms left to ask
        best_match(Patient, BM),
        ( BM \= none ->
            Final = BM,
            format('Diagnosis (best match): ~w~n', [BM])
        ;
            Final = unknown,
            writeln('Unable to determine diagnosis.')
        )
      )
    ).

% ------------------------
% Root entrypoint: fever-based pre-screen, then entropy-driven questions as per Problem statement
% ------------------------
start_decision_tree(Patient) :-
    clear_patient(Patient),
    writeln('--- Interactive Medical Diagnosis ---'),
    writeln('Use yes. / no. for symptom answers and none. / mild. / high. for fever.'),
    ask_fever(Patient, Fever),
    route_by_fever(Patient, Fever),
    diagnose_loop(Patient, Result),
    format('Final diagnosis for ~w: ~w~n', [Patient, Result]).

% Pre-screen questions depending on the fever answer
% use Patient in the head so the variable in the body is not a singleton
route_by_fever(Patient, mild) :-
    !,
    writeln('Mild fever: checking common cold indicators...'),
    ask_yesno(Patient, runny_nose),
    ask_yesno(Patient, sneezing),
    ask_yesno(Patient, sore_throat),
    ( patient_has(Patient, runny_nose), patient_has(Patient, sneezing) ->
        writeln('Note: runny nose + sneezing suggest common cold (confirming below).')
    ; true ).

route_by_fever(Patient, high) :-
    !,
    writeln('High fever: checking flu/strep indicators...'),
    ask_yesno(Patient, body_aches),
    ask_yesno(Patient, fatigue),
    ask_yesno(Patient, cough),
    ( \+ asked(Patient, sore_throat) -> ask_yesno(Patient, sore_throat) ; true ),
    ( \+ asked(Patient, swollen_lymph_nodes) -> ask_yesno(Patient, swollen_lymph_nodes) ; true ),
    ( \+ asked(Patient, white_spots_on_tonsils) -> ask_yesno(Patient, white_spots_on_tonsils) ; true ).

route_by_fever(Patient, none) :-
    !,
    writeln('No fever: checking allergy indicators...'),
    ( \+ asked(Patient, runny_nose) -> ask_yesno(Patient, runny_nose) ; true ),
    ( \+ asked(Patient, sneezing)   -> ask_yesno(Patient, sneezing)   ; true ),
    ( patient_has(Patient, runny_nose), patient_has(Patient, sneezing) ->
        writeln('Note: runny nose + sneezing without fever suggests allergy (confirming below).')
    ; true ),
    ( \+ asked(Patient, itchy_eyes) -> ask_yesno(Patient, itchy_eyes) ; true ),
    ( \+ asked(Patient, skin_rash)  -> ask_yesno(Patient, skin_rash)  ; true ).

route_by_fever(_, _) :- writeln('Unexpected fever response.').

show_state(Patient) :-
    format('Positive:~n', []),
    forall(patient_has(Patient, S), (write('  + '), writeln(S))),
    format('Negative:~n', []),
    forall(patient_not(Patient, S), (write('  - '), writeln(S))),
    format('Asked:~n', []),
    forall(asked(Patient, S), (write('  ? '), writeln(S))).

