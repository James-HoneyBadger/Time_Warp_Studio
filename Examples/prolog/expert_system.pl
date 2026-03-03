/* =====================================================
   TECH SUPPORT EXPERT SYSTEM in Prolog
   A rule-based diagnostic system for computer problems.
   Demonstrates: rule chaining, interactive queries,
   hypothesis testing, meta-programming, assert.
   ===================================================== */

/* ─── SYMPTOM DATABASE ────────────────────────────────── */

:- dynamic symptom/1.
:- dynamic confirmed/1.

/* ─── DIAGNOSTIC RULES ────────────────────────────────── */

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

/* Storage problems */
diagnosis(disk_full) :-
    has(cannot_save_files),
    has(error_message_disk_full).

diagnosis(disk_failure) :-
    has(clicking_sound),
    has(files_corrupted).

/* Display problems */
diagnosis(driver_crash) :-
    has(blank_screen),
    \+ has(no_power).

diagnosis(overheating) :-
    has(sudden_shutdown),
    has(fan_loud),
    has(hot_to_touch).

/* Boot problems */
diagnosis(boot_file_missing) :-
    has(wont_boot),
    has(missing_os_message).

diagnosis(hardware_failure) :-
    has(wont_boot),
    has(beep_codes),
    \+ has(missing_os_message).

/* ─── SYMPTOM CHECKING ─────────────────────────────────── */

has(Symptom) :-
    confirmed(Symptom), !.
has(Symptom) :-
    \+ symptom(Symptom),
    ask(Symptom).

ask(Symptom) :-
    write('  Do you have this symptom: '),
    write(Symptom),
    write('? (yes/no): '),
    read(Answer),
    process_answer(Answer, Symptom).

process_answer(yes, Symptom) :-
    assert(confirmed(Symptom)).
process_answer(no, Symptom) :-
    assert(symptom(Symptom)),
    fail.

/* ─── SOLUTIONS DATABASE ─────────────────────────────── */

solution(no_internet,
    ['Check router power and cables',
     'Restart your router and modem',
     'Check network adapter settings',
     'Run network troubleshooter']).

solution(dns_problem,
    ['Change DNS to 8.8.8.8 (Google DNS)',
     'Flush DNS: ipconfig /flushdns',
     'Check router DNS settings',
     'Try using 1.1.1.1 (Cloudflare)']).

solution(wifi_password_wrong,
    ['Verify WiFi password is correct',
     'Forget network and reconnect',
     'Check CAPS LOCK when entering password',
     'Reset WiFi password on router']).

solution(high_cpu_usage,
    ['Open Task Manager and check processes',
     'End high-CPU processes',
     'Update or reinstall problematic software',
     'Check for scheduled tasks running']).

solution(memory_shortage,
    ['Close unused applications',
     'Increase virtual memory/page file',
     'Consider adding more RAM',
     'Check for memory leaks with tools']).

solution(malware,
    ['Run antivirus scan immediately',
     'Boot into Safe Mode and scan',
     'Use Malwarebytes removal tool',
     'Consider system restore/reinstall']).

solution(disk_full,
    ['Delete temporary files (Disk Cleanup)',
     'Uninstall unused applications',
     'Move files to external storage',
     'Empty recycle bin']).

solution(disk_failure,
    ['Backup data immediately!',
     'Run SMART diagnostics',
     'Replace hard drive ASAP',
     'Use data recovery software if needed']).

solution(driver_crash,
    ['Restart in Safe Mode',
     'Uninstall and reinstall video drivers',
     'Roll back recent driver update',
     'Check Device Manager for errors']).

solution(overheating,
    ['Clean dust from fans and vents',
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

/* ─── PRINT SOLUTION ─────────────────────────────────── */

print_solution(Problem) :-
    solution(Problem, Steps),
    write(''), nl,
    write('  DIAGNOSIS: '), write(Problem), nl,
    write('  ─────────────────────────────'), nl,
    write('  RECOMMENDED SOLUTIONS:'), nl,
    forall(member(Step, Steps),
           (write('    • '), write(Step), nl)).

/* ─── MAIN DIAGNOSIS PROCEDURE ──────────────────────── */

diagnose :-
    write('  Running diagnosis...'), nl,
    (   diagnosis(Problem)
    ->  print_solution(Problem)
    ;   write('  Could not determine problem.'), nl,
        write('  Please contact technical support.'), nl
    ).

diagnose_all :-
    write('  All detected diagnoses:'), nl,
    forall(diagnosis(P), (write('  - '), write(P), nl)).

/* ─── DEMO RUN ──────────────────────────────────────── */

:- write('╔══════════════════════════════════════╗'), nl.
:- write('║   TECH SUPPORT EXPERT SYSTEM v1.0   ║'), nl.
:- write('╚══════════════════════════════════════╝'), nl.
:- nl.

/* Pre-load a scenario: system is slow with popups */
:- write('  [Demo scenario: slow system with popups]'), nl, nl.
:- assert(confirmed(system_slow)).
:- assert(confirmed(unexpected_popups)).
:- assert(confirmed(antivirus_disabled)).
:- assert(symptom(fan_loud)).
:- assert(symptom(cpu_usage_over_90)).
:- assert(symptom(hard_drive_thrashing)).

:- diagnose_all.
:- diagnose.

:- nl.
:- write('  Expert system complete!'), nl.
