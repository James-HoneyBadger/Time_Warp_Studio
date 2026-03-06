/* =====================================================
   TECH SUPPORT EXPERT SYSTEM in Prolog
   A rule-based diagnostic system for computer problems.
   Demonstrates: rule chaining, facts, assert, write.
   ===================================================== */

/* Symptom database */
:- dynamic symptom/1.
:- dynamic confirmed/1.

/* Network problems */
diagnosis(no_internet) :-
    has(no_network_connectivity),
    has(ping_fails_gateway),
    \+ has(wifi_connected).

diagnosis(dns_problem) :-
    has(no_internet),
    has(ping_ip_works),
    has(ping_hostname_fails).

diagnosis(wifi_password_wrong) :-
    has(wifi_not_connecting),
    has(correct_ssid),
    \+ has(authentication_succeeds).

/* Performance problems */
diagnosis(high_cpu_usage) :-
    has(system_slow),
    has(fan_loud),
    has(cpu_usage_over_90).

diagnosis(memory_shortage) :-
    has(system_slow),
    has(hard_drive_thrashing),
    \+ has(cpu_usage_over_90).

diagnosis(malware) :-
    has(system_slow),
    has(unexpected_popups),
    has(antivirus_disabled).

diagnosis(disk_full) :-
    has(system_slow),
    has(disk_space_low),
    has(error_message_disk_full).

/* Hardware problems */
diagnosis(overheating) :-
    has(random_shutdown),
    has(system_slow),
    has(hot_to_touch).

diagnosis(boot_file_missing) :-
    has(wont_boot),
    has(missing_os_message).

diagnosis(hardware_failure) :-
    has(wont_boot),
    \+ has(missing_os_message).

/* Symptom checking */
has(Symptom) :- confirmed(Symptom).
has(Symptom) :- symptom(Symptom).

/* Solutions database */
solution(no_internet,
    ['Check cable connections',
     'Restart router/modem',
     'Run network troubleshooter']).

solution(dns_problem,
    ['Flush DNS cache',
     'Switch to Google DNS 8.8.8.8',
     'Try using 1.1.1.1 (Cloudflare)']).

solution(wifi_password_wrong,
    ['Verify WiFi password',
     'Forget and reconnect to network',
     'Reset WiFi password on router']).

solution(high_cpu_usage,
    ['Open Task Manager',
     'End resource-heavy processes',
     'Check for scheduled tasks running']).

solution(memory_shortage,
    ['Close unused applications',
     'Add more RAM',
     'Check for memory leaks with tools']).

solution(malware,
    ['Run full antivirus scan',
     'Boot in Safe Mode and scan',
     'Consider system restore/reinstall']).

solution(disk_full,
    ['Empty Recycle Bin',
     'Run Disk Cleanup',
     'Move large files to external drive']).

solution(overheating,
    ['Clean dust from vents',
     'Ensure adequate ventilation',
     'Replace thermal paste on CPU',
     'Check fan operation in BIOS']).

solution(boot_file_missing,
    ['Boot from installation media',
     'Run startup repair',
     'Rebuild BCD with bootrec /rebuildbcd',
     'Check hard drive for errors']).

solution(hardware_failure,
    ['Remove and reseat RAM modules',
     'Test with one RAM stick at a time',
     'Check all cable connections',
     'POST test with minimum hardware']).

/* Print solution steps */
print_steps([]).
print_steps([H|T]) :-
    write('    - '), write(H), nl,
    print_steps(T).

print_solution(Problem) :-
    solution(Problem, Steps),
    write(''), nl,
    write('  DIAGNOSIS: '), write(Problem), nl,
    write('  ───────────────────────────────'), nl,
    write('  RECOMMENDED SOLUTIONS:'), nl,
    print_steps(Steps).

/* Diagnosis runner */
diagnose :-
    diagnosis(Problem),
    print_solution(Problem).

/* List all diagnoses using fail-driven loop */
print_all_diagnoses :-
    diagnosis(P),
    write('  - '), write(P), nl,
    fail.
print_all_diagnoses.

diagnose_all :-
    write('  All detected diagnoses:'), nl,
    print_all_diagnoses.

/* Demo run */
:- write('╔══════════════════════════════════════╗'), nl.
:- write('║   TECH SUPPORT EXPERT SYSTEM v1.0   ║'), nl.
:- write('╚══════════════════════════════════════╝'), nl.
:- nl.

/* Pre-load a scenario: system is slow with popups */
:- write('  [Demo scenario: slow system with popups]'), nl, nl.
:- assert(confirmed(system_slow)).
:- assert(confirmed(unexpected_popups)).
:- assert(confirmed(antivirus_disabled)).

:- diagnose_all.
:- diagnose.

:- nl.
:- write('  Expert system complete!'), nl.
