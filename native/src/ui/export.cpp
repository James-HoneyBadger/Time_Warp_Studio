// SPDX-License-Identifier: MIT

#include "timewarp/ui/export.hpp"

#include <QBuffer>
#include <QPageLayout>
#include <QPageSize>
#include <QPdfWriter>
#include <QSaveFile>
#include <QStringList>
#include <QTextDocument>

namespace timewarp::ui::exporter {

namespace {

[[nodiscard]] QFont codeFont(int points) {
    QFont font{QStringLiteral("Courier New")};
    font.setStyleHint(QFont::Monospace);
    font.setFixedPitch(true);
    font.setPointSize(points);
    return font;
}

/// Qt stamps every PDF it writes with the moment it was made, and sometimes with
/// a random document id. Both are metadata rather than content, and leaving them
/// in means the same program exported twice produces two different files -- which
/// makes "did the export change?" impossible to answer and the determinism test
/// impossible to write. They are replaced with fixed values.
void normalisePdfMetadata(QByteArray& pdf) {
    const auto stamp = [&pdf](const char* key) {
        const QByteArray name{key};
        const int at = pdf.indexOf(name);
        if (at < 0) {
            return;
        }
        const int open = pdf.indexOf('(', at + name.size());
        if (open < 0) {
            return;
        }
        const int close = pdf.indexOf(')', open);
        if (close < 0) {
            return;
        }
        pdf.replace(open + 1, close - open - 1, QByteArrayLiteral("D:19700101000000Z"));
    };
    stamp("/CreationDate");
    stamp("/ModDate");

    // The XMP block names the document by a fresh UUID on every write, which is
    // the other thing that would make two exports of one program differ.
    const QByteArray uuid_key = QByteArrayLiteral("uuid:");
    const QByteArray fixed_uuid = QByteArrayLiteral("00000000-0000-0000-0000-000000000000");
    for (int at = pdf.indexOf(uuid_key); at >= 0; at = pdf.indexOf(uuid_key, at + 1)) {
        const int start = at + uuid_key.size();
        // A canonical UUID is exactly 36 characters.
        if (start + fixed_uuid.size() <= pdf.size()) {
            pdf.replace(start, fixed_uuid.size(), fixed_uuid);
        }
    }

    // The same timestamps again, in XMP's ISO form, inside quoted attributes.
    const auto quoted = [&pdf](const QByteArray& key) {
        const QByteArray fixed_stamp = QByteArrayLiteral("1970-01-01T00:00:00Z");
        for (int at = pdf.indexOf(key); at >= 0; at = pdf.indexOf(key, at + 1)) {
            // `key` ends with the attribute's opening quote, so the value starts
            // just past it and ends at the next quote.
            const int open = at + key.size() - 1;
            const int close = pdf.indexOf('"', open + 1);
            if (close < 0) {
                return;
            }
            pdf.replace(open + 1, close - open - 1, fixed_stamp);
            at = close;
        }
    };
    quoted(QByteArrayLiteral("xmp:CreateDate=\""));
    quoted(QByteArrayLiteral("xmp:ModifyDate=\""));
    quoted(QByteArrayLiteral("xmp:MetadataDate=\""));

    // `/ID [<hex><hex>]` is derived from the file's identity, not its content.
    const QByteArray id_key = QByteArrayLiteral("/ID");
    const int id_at = pdf.indexOf(id_key);
    if (id_at >= 0) {
        const int open = pdf.indexOf('[', id_at);
        const int close = pdf.indexOf(']', open);
        if (open >= 0 && close > open) {
            pdf.replace(open + 1, close - open - 1,
                        QByteArrayLiteral("<00000000000000000000000000000000>"
                                          "<00000000000000000000000000000000>"));
        }
    }
}

/// The body of a `QTextDocument::toHtml()` result, without its `<html>`/`<body>`
/// wrapper. The document is what escapes and wraps the program text; the page
/// this returns into is what makes it self-contained, and nesting one full HTML
/// document inside another is invalid.
[[nodiscard]] QString htmlBody(const QString& document_html) {
    const int body = document_html.indexOf(QStringLiteral("<body"));
    if (body < 0) {
        return document_html;
    }
    const int start = document_html.indexOf(QLatin1Char('>'), body);
    const int end = document_html.lastIndexOf(QStringLiteral("</body>"));
    if (start < 0 || end <= start) {
        return document_html;
    }
    return document_html.mid(start + 1, end - start - 1);
}

[[nodiscard]] QString codePageHtml(const QString& title, const QString& language,
                                   const QString& source) {
    QStringList lines = source.split(QLatin1Char('\n'));
    // A trailing newline is the end of the last line, not an extra blank one.
    if (!lines.isEmpty() && lines.constLast().isEmpty()) {
        lines.removeLast();
    }
    if (lines.isEmpty()) {
        lines.append(QString{});
    }

    const int digits = QString::number(lines.size()).size();
    QString text;
    text.reserve(source.size() + static_cast<int>(lines.size()) * (digits + 2));
    for (int index = 0; index < lines.size(); ++index) {
        QString line = lines.at(index);
        line.replace(QLatin1Char('\t'), QStringLiteral("    "));
        text += QString::number(index + 1).rightJustified(digits, QLatin1Char(' '));
        text += QStringLiteral("  ");
        text += line;
        text += QLatin1Char('\n');
    }

    return QStringLiteral(
               "<p style=\"font-family:'Helvetica',sans-serif;font-size:15pt;"
               "font-weight:600;margin:0 0 2pt 0\">%1</p>"
               "<p style=\"font-family:'Helvetica',sans-serif;font-size:9pt;"
               "color:#666666;margin:0 0 12pt 0\">%2 &middot; %3 line%4</p>"
               "<pre style=\"font-family:'Courier New',monospace;font-size:9pt;"
               "margin:0\">%5</pre>")
        .arg(title.toHtmlEscaped(), language.toHtmlEscaped(),
             QString::number(lines.size()), lines.size() == 1 ? QString{} : QStringLiteral("s"),
             text.toHtmlEscaped());
}

[[nodiscard]] bool writeBytes(const QString& path, const QByteArray& bytes, QString* error) {
    QSaveFile file{path};
    if (!file.open(QIODevice::WriteOnly)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    if (file.write(bytes) != bytes.size() || !file.commit()) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    return true;
}

}  // namespace

bool writeCodePdf(const QString& path, const QString& title, const QString& language,
                  const QString& source, QString* error) {
    QByteArray bytes;
    QBuffer buffer{&bytes};
    if (!buffer.open(QIODevice::WriteOnly)) {
        if (error != nullptr) {
            *error = buffer.errorString();
        }
        return false;
    }

    {
        QPdfWriter writer{&buffer};
        writer.setPageSize(QPageSize{QPageSize::A4});
        writer.setPageMargins(QMarginsF{15.0, 15.0, 15.0, 15.0}, QPageLayout::Millimeter);
        // Enough resolution that 9-point code is crisp when printed; the font
        // sizes above are in points, so the text stays the same physical size.
        writer.setResolution(300);
        writer.setTitle(title);
        writer.setCreator(QStringLiteral("Time Warp Studio"));

        QTextDocument document;
        document.setDefaultFont(codeFont(9));
        document.setHtml(codePageHtml(title, language, source));
        document.print(&writer);
    }
    buffer.close();

    normalisePdfMetadata(bytes);
    return writeBytes(path, bytes, error);
}

bool writeSessionHtml(const QString& path, const QString& title, const QString& language,
                      const QString& source, const QString& console_output,
                      const QImage& drawing, QString* error) {
    // The program goes through a QTextDocument rather than a hand-rolled escape:
    // that is what the text views already use, so the export and the editor agree
    // on how a tab, a quote or a Unicode character is represented.
    QTextDocument code;
    code.setDefaultFont(codeFont(10));
    code.setPlainText(source);

    QString drawing_markup;
    if (drawing.isNull()) {
        drawing_markup = QStringLiteral("<p class=\"meta\">This session produced no drawing.</p>");
    } else {
        QByteArray png;
        QBuffer buffer{&png};
        buffer.open(QIODevice::WriteOnly);
        drawing.save(&buffer, "PNG");
        buffer.close();
        drawing_markup = QStringLiteral("<img alt=\"The program's drawing\" src=\"data:image/png;base64,") +
                         QString::fromLatin1(png.toBase64()) + QStringLiteral("\">");
    }

    const QString page =
        QStringLiteral(
            "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
            "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
            "<title>%1</title>\n<style>\n"
            "body{font-family:system-ui,-apple-system,'Segoe UI',sans-serif;margin:2rem auto;"
            "max-width:60rem;padding:0 1rem;color:#1a1a1a;line-height:1.5}\n"
            "h1{font-size:1.6rem;margin:0 0 .25rem}\n"
            ".meta{color:#666;margin:0 0 2rem}\n"
            "h2{font-size:1.1rem;margin:2rem 0 .5rem;border-bottom:1px solid #ddd;"
            "padding-bottom:.25rem}\n"
            "pre{background:#f6f8fa;padding:1rem;overflow:auto;border-radius:6px;"
            "font-family:ui-monospace,'Courier New',monospace;font-size:.9rem}\n"
            "img{max-width:100%;height:auto;border:1px solid #ddd;border-radius:6px;"
            "background:#fff}\n"
            "</style>\n</head>\n<body>\n"
            "<h1>%1</h1>\n<p class=\"meta\">%2</p>\n"
            "<h2>Program</h2>\n%3\n"
            "<h2>Drawing</h2>\n%4\n"
            "<h2>Output</h2>\n<pre>%5</pre>\n"
            "</body>\n</html>\n")
            .arg(title.toHtmlEscaped(), language.toHtmlEscaped(), htmlBody(code.toHtml()),
                 drawing_markup, console_output.toHtmlEscaped());

    return writeBytes(path, page.toUtf8(), error);
}

}  // namespace timewarp::ui::exporter
