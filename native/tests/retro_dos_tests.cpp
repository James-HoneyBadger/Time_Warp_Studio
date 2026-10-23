// SPDX-License-Identifier: MIT
//
// HB DOS: saving, the catalogue, loading, running, the lock bit, renaming, TYPE,
// VERIFY and the twice-said INIT.

#include "retro/support.hpp"

namespace tw::retro {

void test_hb_dos_saves_catalogues_loads_and_runs() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"FROM DISK\""));

    machine.submitCommand(QStringLiteral("SAVE \"FIRST\""));
    TW_CHECK(machine.disk().find(QStringLiteral("FIRST")) != nullptr);

    machine.submitCommand(QStringLiteral("CATALOG"));
    const QString catalog = machine.consoleView()->toPlainText();
    TW_CHECK(catalog.contains(QStringLiteral("DISK VOLUME 254")));
    TW_CHECK(catalog.contains(QStringLiteral("HBWARP")));
    TW_CHECK(catalog.contains(QStringLiteral("BASIC")));
    TW_CHECK(catalog.contains(QStringLiteral("001")));  // one sector
    TW_CHECK(catalog.contains(QStringLiteral("FIRST")));
    TW_CHECK(catalog.contains(QStringLiteral("1 FILES")));

    machine.submitCommand(QStringLiteral("NEW"));
    TW_CHECK(machine.program().trimmed().isEmpty());

    machine.submitCommand(QStringLiteral("LOAD \"FIRST\""));
    TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"FROM DISK\""));

    machine.submitCommand(QStringLiteral("RUN \"FIRST\""));
    TW_CHECK(spin_until([&machine] {
        return machine.consoleView()->toPlainText().contains(QStringLiteral("FROM DISK"));
    }));
}


void test_hb_dos_lock_protects_a_file() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"KEEP\""));
    machine.submitCommand(QStringLiteral("SAVE \"KEEP\""));

    machine.submitCommand(QStringLiteral("LOCK \"KEEP\""));
    const DosFile* file = machine.disk().find(QStringLiteral("KEEP"));
    TW_CHECK(file != nullptr);
    TW_CHECK(file != nullptr && file->locked);

    machine.submitCommand(QStringLiteral("DELETE \"KEEP\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("?FILE LOCKED")));
    TW_CHECK(machine.disk().find(QStringLiteral("KEEP")) != nullptr);

    machine.submitCommand(QStringLiteral("CATALOG"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("*BASIC")));

    machine.submitCommand(QStringLiteral("UNLOCK \"KEEP\""));
    TW_CHECK(machine.disk().find(QStringLiteral("KEEP")) != nullptr);
    TW_CHECK(!machine.disk().find(QStringLiteral("KEEP"))->locked);
    machine.submitCommand(QStringLiteral("DELETE \"KEEP\""));
    TW_CHECK(machine.disk().find(QStringLiteral("KEEP")) == nullptr);
}


void test_hb_dos_renames_and_types() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"TYPED\""));

    machine.submitCommand(QStringLiteral("SAVE \"OLD\""));
    machine.submitCommand(QStringLiteral("RENAME \"OLD\",\"NEW\""));
    TW_CHECK(machine.disk().find(QStringLiteral("OLD")) == nullptr);
    TW_CHECK(machine.disk().find(QStringLiteral("NEW")) != nullptr);

    machine.submitCommand(QStringLiteral("TYPE \"NEW\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("10 PRINT \"TYPED\"")));

    // A rename onto a name that is taken is refused, not merged.
    machine.submitCommand(QStringLiteral("SAVE \"OTHER\""));
    machine.submitCommand(QStringLiteral("RENAME \"OTHER\",\"NEW\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("?FILE EXISTS")));
    TW_CHECK(machine.disk().find(QStringLiteral("OTHER")) != nullptr);
}


void test_hb_dos_verify_compares_memory_with_the_disk() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"SAME\""));
    machine.submitCommand(QStringLiteral("SAVE \"CHECK\""));

    machine.submitCommand(QStringLiteral("VERIFY \"CHECK\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("CHECK VERIFIED")));

    machine.loadProgram(QStringLiteral("20 PRINT \"CHANGED\""));
    machine.submitCommand(QStringLiteral("VERIFY \"CHECK\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("?CHECK DIFFERS FROM MEMORY")));
}


void test_hb_dos_init_has_to_be_said_twice() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"DOOMED\""));
    machine.submitCommand(QStringLiteral("SAVE \"DOOMED\""));

    machine.submitCommand(QStringLiteral("INIT"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("TYPE INIT AGAIN TO GO AHEAD")));
    TW_CHECK(machine.disk().find(QStringLiteral("DOOMED")) != nullptr);

    // Any other word is a change of mind: the next INIT warns again.
    machine.submitCommand(QStringLiteral("CATALOG"));
    machine.submitCommand(QStringLiteral("INIT"));
    TW_CHECK(machine.disk().find(QStringLiteral("DOOMED")) != nullptr);

    machine.submitCommand(QStringLiteral("INIT"));
    TW_CHECK(machine.disk().find(QStringLiteral("DOOMED")) == nullptr);
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("ALL FILES ERASED")));
}


void test_hb_dos_survives_a_restart() {
    clearMachineSettings();
    {
        RetroComputerWindow machine;
        machine.loadProgram(QStringLiteral("20 PRINT \"PERSIST\""));
        machine.submitCommand(QStringLiteral("SAVE \"PERSIST\""));
        TW_CHECK(machine.disk().find(QStringLiteral("PERSIST")) != nullptr);
    }
    {
        RetroComputerWindow machine;
        TW_CHECK(machine.disk().find(QStringLiteral("PERSIST")) != nullptr);
        machine.submitCommand(QStringLiteral("CATALOG"));
        TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("PERSIST")));
        TW_CHECK(machine.statusLabel()->text().contains(QStringLiteral("HB DOS 1F")));
    }
}

}  // namespace tw::retro
