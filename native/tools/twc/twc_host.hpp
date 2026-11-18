// SPDX-License-Identifier: AGPL-3.0-only
//
// The compiler's name for the shared recording host. The implementation lives in
// `timewarp/graphics/recording_host.hpp` because the IDE's API server records the
// same drawing; one recorder means a compiled program and an API run cannot
// disagree about what a turtle command drew.

#pragma once

#include "timewarp/graphics/recording_host.hpp"

using TwcHost = timewarp::graphics::RecordingHost;
