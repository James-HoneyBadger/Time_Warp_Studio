// SPDX-License-Identifier: MIT

#include "timewarp/hb2/shell.hpp"

#include "timewarp/hb2/rom.hpp"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <sstream>

namespace timewarp::hb2 {

namespace {

std::string upper(std::string_view text) {
    std::string out{text};
    std::transform(out.begin(), out.end(), out.begin(), [](unsigned char character) {
        return static_cast<char>(std::toupper(character));
    });
    return out;
}

std::vector<std::string> split(std::string_view line) {
    std::vector<std::string> parts;
    std::string current;
    for (const char character : line) {
        if (std::isspace(static_cast<unsigned char>(character)) != 0) {
            if (!current.empty()) {
                parts.push_back(current);
                current.clear();
            }
            continue;
        }
        current.push_back(character);
    }
    if (!current.empty()) {
        parts.push_back(current);
    }
    return parts;
}

/// A quoted or bare name, with the quotes removed: `SAVE "HELLO.S"` and `SAVE HELLO.S`
/// are the same request, which is what a period prompt did.
std::string unquote(std::string name) {
    if (name.size() >= 2 && name.front() == '"' && name.back() == '"') {
        name = name.substr(1, name.size() - 2);
    }
    return name;
}

std::string hex(std::uint32_t value, int digits) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    std::string text(static_cast<std::size_t>(digits), '0');
    for (int index = digits - 1; index >= 0; --index) {
        text[static_cast<std::size_t>(index)] = kHex[value & 0xF];
        value >>= 4;
    }
    return text;
}

std::uint32_t number(std::string text) {
    if (!text.empty() && text.front() == '$') {
        return static_cast<std::uint32_t>(std::stoul(text.substr(1), nullptr, 16));
    }
    return static_cast<std::uint32_t>(std::stoul(text));
}

}  // namespace

Shell::Shell(Machine& machine) : machine_(machine) {}

std::string Shell::prompt() const { return std::string(1, drive_ == 0 ? 'A' : 'B') + ">"; }

std::vector<std::string> Shell::words() {
    return {
        "HELP                  this list",
        "DIR [path]            the catalogue of the current directory",
        "CD <path>             change directory          MKDIR <path>   make one",
        "RMDIR <path>          remove an empty directory  DRIVE <0|1>    A: or B:",
        "TYPE <file>           print a file              DELETE <file>  remove it",
        "COPY <from> <to>      copy                      MOVE <from> <to>  move it",
        "RENAME <from> <to>    rename                    LOCK/UNLOCK <file>",
        "FORMAT [0|1]          format a drive            FREE           the sector map",
        "LIST [file]           the listing in the editor, or a file",
        "NEW                   clear the listing         SAVE [file]    the listing to disk",
        "LOAD <file>           a file into the listing",
        "RUN [file]            assemble and run          SPAWN [file]   run as a task",
        "TASKS                 the processes             KILL           stop them all",
        "REGS                  the registers             DIS [addr]     disassemble",
        "DUMP [addr]           memory                    POKE <addr> <byte>...",
        "STEP                  one instruction           BREAK <addr>   a breakpoint",
        "RUNTO                 run to a breakpoint",
        "ROM [name]            the socket: HB16, or one of the nine engines",
        "TAPE                  the cassette and where the head is",
        "CSAVE <name>          record the listing      CLOAD <name>  play it back",
        "REWIND                put the head at the start    FORWARD  play past a file",
        "MODULE [list|save <n>|pack <n>|boot <n>]  the module bay",
        "SERIAL <text>         write the serial port     TIME           the machine's clock",
        "CLS                   clear the screen",
    };
}

bool Shell::requireDisk(std::vector<std::string>* out) const {
    if (dos_ == nullptr) {
        out->push_back("?NO DISK");
        return false;
    }
    return true;
}

std::vector<std::string> Shell::execute(std::string_view line) {
    std::vector<std::string> out;
    const std::vector<std::string> parts = split(line);
    if (parts.empty()) {
        return out;
    }
    const std::string verb = upper(parts[0]);
    std::vector<std::string> args;
    for (std::size_t index = 1; index < parts.size(); ++index) {
        args.push_back(unquote(parts[index]));
    }

    if (verb == "HELP" || verb == "?") {
        return words();
    }
    if (verb == "CLS") {
        machine_.resetVideoSurface();
        return {"screen cleared"};
    }
    if (verb == "TIME") {
        out.push_back("RTC " + std::to_string(machine_.rtc().read(Rtc::kYear)) + "-" +
                      hex(machine_.rtc().read(Rtc::kMonth), 2) + "-" +
                      hex(machine_.rtc().read(Rtc::kDay), 2) + " " +
                      hex(machine_.rtc().read(Rtc::kHours), 2) + ":" +
                      hex(machine_.rtc().read(Rtc::kMinutes), 2) + ":" +
                      hex(machine_.rtc().read(Rtc::kSeconds), 2));
        return out;
    }
    if (verb == "SERIAL") {
        std::string text;
        for (std::size_t index = 1; index < parts.size(); ++index) {
            if (!text.empty()) {
                text.push_back(' ');
            }
            text += parts[index];
        }
        machine_.uart().receive("");
        for (const char character : text) {
            machine_.uart().write(Uart::kData, static_cast<std::uint8_t>(character));
        }
        out.push_back("sent " + std::to_string(text.size()) + " byte(s)");
        return out;
    }

    if (verb == "DIR" || verb == "CD" || verb == "CHDIR" || verb == "MKDIR" || verb == "MD" ||
        verb == "RMDIR" || verb == "RD" || verb == "TYPE" || verb == "DELETE" || verb == "DEL" ||
        verb == "COPY" || verb == "MOVE" || verb == "RENAME" || verb == "LOCK" ||
        verb == "UNLOCK" || verb == "FORMAT" || verb == "FREE" || verb == "DRIVE" ||
        verb == "SAVE" || verb == "LOAD") {
        return diskWords(verb, args);
    }

    if (verb == "LIST") {
        const std::string text = args.empty() ? source_ : std::string{};
        if (!args.empty()) {
            if (!requireDisk(&out)) {
                return out;
            }
            const std::vector<std::uint8_t> bytes = dos_->readFile(drive_, args[0]);
            if (!dos_->error().empty()) {
                return {dos_->error()};
            }
            out.push_back(std::string(bytes.begin(), bytes.end()));
            return out;
        }
        std::istringstream lines{text};
        std::string row;
        while (std::getline(lines, row)) {
            out.push_back(row);
        }
        if (out.empty()) {
            out.push_back("(the listing is empty)");
        }
        return out;
    }
    if (verb == "NEW") {
        source_.clear();
        return {"listing cleared"};
    }
    if (verb == "RUN" || verb == "SPAWN") {
        return runListing(args.empty() ? std::string{} : args[0], verb == "SPAWN");
    }

    if (verb == "TASKS") {
        const std::vector<std::string> rows = machine_.scheduler().status();
        if (rows.empty()) {
            return {"no processes"};
        }
        return rows;
    }
    if (verb == "KILL") {
        machine_.stopAllTasks();
        return {"processes stopped"};
    }

    if (verb == "REGS" && monitor_ != nullptr) {
        return {monitor_->registers()};
    }
    if ((verb == "DIS" || verb == "DUMP") && monitor_ != nullptr) {
        const std::uint32_t address = args.empty() ? machine_.cpu().registers().pc : number(args[0]);
        return verb == "DIS" ? monitor_->disassemble(address, 8) : monitor_->dump(address, 8);
    }
    if (verb == "POKE" && monitor_ != nullptr && args.size() >= 2) {
        const std::uint32_t address = number(args[0]);
        std::vector<std::uint8_t> bytes;
        for (std::size_t index = 1; index < args.size(); ++index) {
            bytes.push_back(static_cast<std::uint8_t>(number(args[index]) & 0xFF));
        }
        monitor_->poke(address, bytes);
        return {"poked " + std::to_string(bytes.size()) + " byte(s) at $" + hex(address, 6)};
    }
    if (verb == "STEP" && monitor_ != nullptr) {
        std::string what;
        monitor_->step(&what);
        out.push_back("stepped  " + what);
        out.push_back(monitor_->registers());
        return out;
    }
    if (verb == "BREAK" && monitor_ != nullptr && !args.empty()) {
        const std::uint32_t address = number(args[0]);
        const bool on = !monitor_->breakpointAt(address);
        monitor_->setBreakpoint(address, on);
        return {(on ? "breakpoint set at $" : "breakpoint cleared at $") + hex(address, 6)};
    }
    if (verb == "RUNTO" && monitor_ != nullptr) {
        return {monitor_->runToBreakpoint()};
    }

    if (verb == "ROM") {
        return romWords(args);
    }
    if (verb == "TAPE" || verb == "CSAVE" || verb == "CLOAD" || verb == "REWIND" ||
        verb == "FORWARD") {
        return tapeWords(verb, args);
    }

    if (verb == "MODULE" && modules_ != nullptr) {
        const std::string action = args.empty() ? "list" : upper(args[0]);
        if (action == "LIST") {
            const std::vector<ModuleRow> rows = modules_->list();
            if (rows.empty()) {
                return {"the bay is empty: " + modules_->directory()};
            }
            for (const ModuleRow& row : rows) {
                out.push_back(row.name + std::string(row.name.size() < 16 ? 16 - row.name.size() : 1, ' ') +
                              row.kind);
            }
            return out;
        }
        if (action == "SAVE" && args.size() >= 2) {
            // What is saved is the listing in the editor: a script module is the
            // sequence of words a person would type, and there is one obvious source.
            std::vector<std::string> lines;
            std::istringstream stream{source_};
            std::string row;
            while (std::getline(stream, row)) {
                lines.push_back(row);
            }
            if (!modules_->saveScript(args[1], lines)) {
                return {modules_->lastError()};
            }
            return {"saved module " + args[1]};
        }
        if (action == "PACK" && args.size() >= 2) {
            // A package is what is running, taken down as the listings it was started
            // from: a picture of registers would be a machine frozen mid-instruction,
            // and what a person wants to keep is the programs.
            std::vector<ModuleProgram> programs;
            for (const Task& task : machine_.scheduler().tasks()) {
                ModuleProgram program;
                program.name = task.name;
                program.source = task.source;
                programs.push_back(std::move(program));
            }
            if (!modules_->savePackage(args[1], programs)) {
                return {modules_->lastError()};
            }
            return {"packed " + std::to_string(programs.size()) + " program(s) into " + args[1]};
        }
        if (action == "BOOT" && args.size() >= 2) {
            ModuleContent content;
            if (!modules_->read(args[1], &content)) {
                return {modules_->lastError()};
            }
            if (content.kind == "script") {
                for (const std::string& script_line : content.script) {
                    const std::vector<std::string> inner = execute(script_line);
                    out.insert(out.end(), inner.begin(), inner.end());
                }
                return out;
            }
            if (content.kind == "binary") {
                machine_.loadProgram(content.binary, content.entry);
                machine_.run(200000);
                out.push_back("booted " + args[1] + ": " + machine_.statusText());
                return out;
            }
            if (content.kind == "image") {
                out.push_back(machine_.restore(content.image) ? "booted " + args[1]
                                                             : "?IMAGE REFUSED");
                return out;
            }
            if (content.kind == "package") {
                // Onto the machine as it stands: nothing is cleared, nothing resident is
                // stopped, and a program that does not fit is refused by name.
                for (const ModuleProgram& program : content.programs) {
                    const int id = machine_.spawn(program.name, program.source);
                    if (id != 0) {
                        out.push_back("started " + program.name + " as process " +
                                      std::to_string(id));
                        continue;
                    }
                    std::string why = machine_.scheduler().lastError();
                    if (!why.empty() && why.front() == '?') {
                        why = why.substr(1);
                    }
                    out.push_back("? " + program.name + ": " + why);
                }
                return out;
            }
        }
        return {"MODULE list | save <name> | pack <name> | boot <name>"};
    }

    out.push_back("?WHAT: " + verb + " -- HELP lists the words");
    return out;
}

std::vector<std::string> Shell::diskWords(const std::string& verb,
                                          const std::vector<std::string>& args) {
    std::vector<std::string> out;
    if (!requireDisk(&out)) {
        return out;
    }
    if (verb == "DRIVE") {
        setDrive(args.empty() ? 0 : static_cast<int>(number(args[0])));
        return {"drive " + std::string(1, drive_ == 0 ? 'A' : 'B') + ": " + dos_->volumeName(drive_)};
    }
    if (verb == "FORMAT") {
        const int drive = args.empty() ? drive_ : static_cast<int>(number(args[0]));
        return {dos_->format(drive) ? "formatted drive " + std::to_string(drive) + " as " +
                                          dos_->volumeName(drive)
                                    : dos_->error()};
    }
    if (verb == "FREE") {
        const DosFree space = dos_->free(drive_);
        out.push_back("volume " + dos_->volumeName(drive_) + "  " + std::to_string(space.sectors) +
                      " sectors, " + std::to_string(space.used) + " used, " +
                      std::to_string(space.free) + " free");
        out.insert(out.end(), space.map.begin(), space.map.end());
        return out;
    }
    if (verb == "DIR") {
        const std::vector<DosRow> rows = dos_->list(drive_, args.empty() ? std::string{} : args[0], true);
        if (!dos_->error().empty()) {
            return {dos_->error()};
        }
        out.push_back(dos_->currentDirectory(drive_) + "  " + dos_->volumeName(drive_));
        for (const DosRow& row : rows) {
            std::string line = row.name;
            if (line.size() < 20) {
                line.append(20 - line.size(), ' ');
            }
            if (row.directory) {
                line += "<DIR>";
            } else {
                line += std::to_string(row.bytes);
            }
            line += "  " + row.stamp.text();
            if (row.read_only) {
                line += " R";
            }
            if (row.locked) {
                line += " L";
            }
            out.push_back(line);
        }
        out.push_back(std::to_string(rows.size()) + " entr" + (rows.size() == 1 ? "y" : "ies"));
        return out;
    }
    if (verb == "CD" || verb == "CHDIR") {
        if (args.empty()) {
            return {dos_->currentDirectory(drive_)};
        }
        return {dos_->setCurrentDirectory(drive_, args[0]) ? dos_->currentDirectory(drive_)
                                                          : dos_->error()};
    }
    if (verb == "MKDIR" || verb == "MD") {
        if (args.empty()) {
            return {"MKDIR needs a name"};
        }
        return {dos_->mkdir(drive_, args[0]) ? "made " + args[0] : dos_->error()};
    }
    if (verb == "RMDIR" || verb == "RD") {
        if (args.empty()) {
            return {"RMDIR needs a name"};
        }
        return {dos_->rmdir(drive_, args[0]) ? "removed " + args[0] : dos_->error()};
    }
    if (verb == "TYPE") {
        if (args.empty()) {
            return {"TYPE needs a file"};
        }
        const std::vector<std::uint8_t> bytes = dos_->readFile(drive_, args[0]);
        if (!dos_->error().empty()) {
            return {dos_->error()};
        }
        std::istringstream lines{std::string(bytes.begin(), bytes.end())};
        std::string row;
        while (std::getline(lines, row)) {
            out.push_back(row);
        }
        return out;
    }
    if (verb == "DELETE" || verb == "DEL") {
        if (args.empty()) {
            return {"DELETE needs a file"};
        }
        return {dos_->removeFile(drive_, args[0]) ? "deleted " + args[0] : dos_->error()};
    }
    if (verb == "COPY" || verb == "MOVE" || verb == "RENAME") {
        if (args.size() < 2) {
            return {verb + " needs two names"};
        }
        const bool ok = verb == "COPY"    ? dos_->copy(drive_, args[0], args[1])
                        : verb == "MOVE"  ? dos_->move(drive_, args[0], args[1])
                                          : dos_->rename(drive_, args[0], args[1]);
        return {ok ? verb + "d " + args[0] + " to " + args[1] : dos_->error()};
    }
    if (verb == "LOCK" || verb == "UNLOCK") {
        if (args.empty()) {
            return {verb + " needs a file"};
        }
        return {dos_->setLocked(drive_, args[0], verb == "LOCK")
                    ? (verb == "LOCK" ? "locked " : "unlocked ") + args[0]
                    : dos_->error()};
    }
    if (verb == "SAVE") {
        const std::string name = args.empty() ? "UNTITLED.S" : args[0];
        const std::vector<std::uint8_t> bytes(source_.begin(), source_.end());
        return {dos_->writeFile(drive_, name, bytes) ? "saved " + name + " (" +
                                                          std::to_string(bytes.size()) + " bytes)"
                                                    : dos_->error()};
    }
    if (verb == "LOAD") {
        if (args.empty()) {
            return {"LOAD needs a file"};
        }
        const std::vector<std::uint8_t> bytes = dos_->readFile(drive_, args[0]);
        if (!dos_->error().empty()) {
            return {dos_->error()};
        }
        source_ = std::string(bytes.begin(), bytes.end());
        return {"loaded " + args[0] + " (" + std::to_string(bytes.size()) + " bytes)"};
    }
    return {"?WHAT: " + verb};
}

std::string Shell::romName() const {
    if (machine_.romIsAssembler()) {
        return "HB16";
    }
    return std::string{lang::display_name(machine_.romLanguage())};
}

std::vector<std::string> Shell::romWords(const std::vector<std::string>& args) {
    if (args.empty()) {
        std::vector<std::string> rows;
        rows.push_back("socket: " + romName());
        std::string line = "  HB16";
        for (const lang::Language language : NativeRom::languages()) {
            line += "  " + std::string{lang::display_name(language)};
        }
        rows.push_back(line);
        return rows;
    }
    const std::string wanted = upper(args[0]);
    if (wanted == "HB16" || wanted == "ASSEMBLER") {
        machine_.setRomAssembler();
        return {"socket: HB16 (the assembler)"};
    }
    for (const lang::Language language : NativeRom::languages()) {
        if (upper(lang::display_name(language)) == wanted || upper(lang::key(language)) == wanted) {
            machine_.setRomLanguage(language);
            // A language prints on the screen, so the screen has to be one it can
            // print on: MODE 0 is the text map, and it is what a language ROM expects.
            if (machine_.video().mode() != Video::Mode::Text) {
                machine_.video().setMode(Video::Mode::Text);
                machine_.bus().setCompat(true);
            }
            return {"socket: " + std::string{lang::display_name(language)}};
        }
    }
    return {"?ROM: no such language -- ROM lists them"};
}

std::vector<std::string> Shell::tapeWords(const std::string& verb,
                                          const std::vector<std::string>& args) {
    std::vector<std::string> out;
    Tape& tape = machine_.tape();
    if (verb == "TAPE") {
        out.push_back("cassette at position " + std::to_string(tape.position()) + " of " +
                      std::to_string(tape.size()));
        const std::vector<std::string> rows = tape.catalog();
        out.insert(out.end(), rows.begin(), rows.end());
        return out;
    }
    if (verb == "REWIND") {
        tape.rewind();
        return {"rewound: position 0 of " + std::to_string(tape.size())};
    }
    if (verb == "FORWARD") {
        if (!tape.forward()) {
            return {tape.error()};
        }
        return {"position " + std::to_string(tape.position()) + " of " + std::to_string(tape.size())};
    }
    if (verb == "CSAVE") {
        const std::string name = args.empty() ? "UNTITLED" : args[0];
        const std::vector<std::uint8_t> bytes(source_.begin(), source_.end());
        if (!tape.record(name, bytes)) {
            return {tape.error()};
        }
        return {"recorded " + name + " (" + std::to_string(bytes.size()) + " bytes) at " +
                std::to_string(tape.size())};
    }
    // CLOAD
    const std::string name = args.empty() ? std::string{} : args[0];
    std::vector<std::uint8_t> bytes;
    if (name.empty()) {
        std::string echoed;
        if (!tape.loadNext(&bytes, &echoed)) {
            return {tape.error()};
        }
        source_ = std::string(bytes.begin(), bytes.end());
        return {"loaded " + echoed + " (" + std::to_string(bytes.size()) + " bytes)"};
    }
    if (!tape.load(name, &bytes)) {
        return {tape.error()};
    }
    source_ = std::string(bytes.begin(), bytes.end());
    return {"loaded " + name + " (" + std::to_string(bytes.size()) + " bytes)"};
}

std::vector<std::string> Shell::runListing(const std::string& file, bool as_task) {
    std::vector<std::string> out;
    std::string listing = source_;
    if (!file.empty()) {
        if (!requireDisk(&out)) {
            return out;
        }
        const std::vector<std::uint8_t> bytes = dos_->readFile(drive_, file);
        if (!dos_->error().empty()) {
            return {dos_->error()};
        }
        listing = std::string(bytes.begin(), bytes.end());
    }
    if (listing.empty()) {
        return {"nothing to run: the listing is empty"};
    }
    if (!machine_.romIsAssembler() && !as_task) {
        // A language ROM: the same engine the editor runs, over this machine's bus, so
        // POKE, LOCATE, COLOR and SYS reach the hardware.
        const std::string printed = machine_.runRom(listing);
        std::istringstream lines{printed};
        std::string row;
        while (std::getline(lines, row)) {
            out.push_back(row);
        }
        if (out.empty()) {
            out.push_back(romName() + " finished");
        }
        return out;
    }
    if (as_task) {
        const int id = machine_.spawn(
            file.empty() ? "TASK" + std::to_string(machine_.scheduler().size() + 1) : file,
            listing);
        if (id == 0) {
            return {machine_.scheduler().lastError()};
        }
        out.push_back("started " + std::to_string(id) + " as a process");
        return out;
    }
    const Assembly assembly = machine_.load(listing);
    if (!assembly.ok) {
        out.push_back("?ASSEMBLY FAILED");
        for (const std::string& error : assembly.errors) {
            out.push_back("  " + error);
        }
        return out;
    }
    machine_.run(2000000);
    out.push_back(machine_.statusText());
    if (!machine_.console().empty()) {
        std::istringstream lines{machine_.console()};
        std::string row;
        while (std::getline(lines, row)) {
            out.push_back(row);
        }
    }
    return out;
}

}  // namespace timewarp::hb2
