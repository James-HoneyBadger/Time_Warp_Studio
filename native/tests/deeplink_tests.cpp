// SPDX-License-Identifier: AGPL-3.0-only
//
// The deep-link parser: the shareable-link format is a URL, and its meaning must
// not depend on a window existing, so it is tested against QtCore alone.

#include <QByteArray>
#include <QString>

#include "check.hpp"
#include "timewarp/ui/deeplink.hpp"

namespace {

using timewarp::ui::DeepLink;
using timewarp::ui::parseDeepLink;

/// URL-safe base64, the alphabet the links are written in.
QString encode(const QByteArray& bytes) {
    return QString::fromLatin1(bytes.toBase64(QByteArray::Base64UrlEncoding |
                                              QByteArray::OmitTrailingEquals));
}

void test_empty_query() {
    TW_CHECK(parseDeepLink(QString{}).isEmpty());
    TW_CHECK(parseDeepLink(QStringLiteral("?")).isEmpty());
    TW_CHECK(parseDeepLink(QStringLiteral("?ignored=1")).isEmpty());
}

void test_example() {
    const DeepLink link =
        parseDeepLink(QStringLiteral("example=logo/mandala.logo"));
    TW_CHECK_EQ(link.example, QStringLiteral("logo/mandala.logo"));
    TW_CHECK(!link.isEmpty());
    TW_CHECK(!link.run);
}

void test_example_is_percent_decoded() {
    const DeepLink link =
        parseDeepLink(QStringLiteral("?example=chip8%2Fpong.ch8"));
    TW_CHECK_EQ(link.example, QStringLiteral("chip8/pong.ch8"));
}

void test_shared_program() {
    const QByteArray source = "10 PRINT \"HELLO\"";
    const DeepLink link =
        parseDeepLink(QStringLiteral("lang=basic&code=") + encode(source));
    TW_CHECK_EQ(link.language, QStringLiteral("basic"));
    TW_CHECK_EQ(link.source, QStringLiteral("10 PRINT \"HELLO\""));
    TW_CHECK(!link.isEmpty());
}

void test_standard_base64_is_accepted() {
    const QByteArray source = "1 + 1";
    const QString standard =
        QString::fromLatin1(source.toBase64());  // standard alphabet, padded
    const DeepLink link = parseDeepLink(QStringLiteral("lang=apl&code=") + standard);
    TW_CHECK_EQ(link.language, QStringLiteral("apl"));
    TW_CHECK_EQ(link.source, QStringLiteral("1 + 1"));
}

void test_code_without_language_is_ignored() {
    const DeepLink link =
        parseDeepLink(QStringLiteral("code=") + encode("1 + 1"));
    TW_CHECK_EQ(link.language, QString{});
    TW_CHECK_EQ(link.source, QString{});
    TW_CHECK(link.isEmpty());
}

void test_notebook_and_run() {
    const DeepLink link = parseDeepLink(
        QStringLiteral("?notebook=https%3A%2F%2Fexample.org%2Flesson.twnb&run=1"));
    TW_CHECK_EQ(link.notebook, QStringLiteral("https://example.org/lesson.twnb"));
    TW_CHECK(link.run);
    TW_CHECK(!link.isEmpty());
}

void test_parameters_before_the_question_mark_are_accepted() {
    const DeepLink with = parseDeepLink(QStringLiteral("?lang=logo"));
    const DeepLink without = parseDeepLink(QStringLiteral("lang=logo"));
    TW_CHECK_EQ(with.language, QStringLiteral("logo"));
    TW_CHECK_EQ(without.language, QStringLiteral("logo"));
}

}  // namespace

int main() {
    test_empty_query();
    test_example();
    test_example_is_percent_decoded();
    test_shared_program();
    test_standard_base64_is_accepted();
    test_code_without_language_is_ignored();
    test_notebook_and_run();
    test_parameters_before_the_question_mark_are_accepted();
    return tw::test::summary("deeplink");
}
