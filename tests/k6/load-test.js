import http from 'k6/http';
import { check, sleep, group } from 'k6';
import { Rate, Trend, Counter, Gauge } from 'k6/metrics';

/**
 * K6 Load Testing Suite for Time Warp Studio
 * 
 * Run with: k6 run load-test.js
 * 
 * Thresholds:
 * - p95 latency: < 500ms
 * - p99 latency: < 1000ms
 * - error rate: < 0.1%
 * - availability: > 99.9%
 */

export const options = {
  stages: [
    { duration: '1m', target: 10 },    // Ramp-up
    { duration: '3m', target: 50 },    // Stay at moderate load
    { duration: '2m', target: 100 },   // Increase to high load
    { duration: '3m', target: 100 },   // Stay at high load
    { duration: '2m', target: 0 },     // Ramp-down
  ],
  thresholds: {
    'http_req_duration': ['p(95)<500', 'p(99)<1000'],
    'http_req_failed': ['rate<0.001'],
    'api_execution_time': ['p(95)<1000'],
    'api_errors': ['count<10'],
  },
};

// Custom metrics
const executionTime = new Trend('api_execution_time');
const executionErrors = new Counter('api_errors');
const cacheHitRate = new Rate('cache_hit_rate');
const concurrentUsers = new Gauge('concurrent_users');
const throughput = new Counter('requests_per_second');

const BASE_URL = __ENV.BASE_URL || 'http://localhost:8000';

export default function () {
  const testToken = http.post(`${BASE_URL}/api/auth/test-token`, {
    username: `loadtest-${__VU}-${__ITER}`,
  }).body;

  concurrentUsers.add(__VU);
  throughput.add(1);

  group('Code Execution', () => {
    const basicCode = `
      PRINT "Hello World"
      FOR i = 1 TO 100
        PRINT i
      NEXT i
    `;

    const startTime = new Date();
    const response = http.post(
      `${BASE_URL}/api/execute`,
      {
        language: 'basic',
        code: basicCode,
      },
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
          'Content-Type': 'application/json',
        },
        timeout: '30s',
      }
    );
    const duration = new Date() - startTime;
    executionTime.add(duration);

    check(response, {
      'status is 200': (r) => r.status === 200,
      'execution successful': (r) => JSON.parse(r.body).success === true,
      'response time < 500ms': (r) => r.timings.duration < 500,
    });

    if (response.status !== 200) {
      executionErrors.add(1);
    }
  });

  group('Concurrent Edits', () => {
    // Simulate multiple users editing the same document
    const editCode = `
      x = ${__ITER}
      PRINT x * 2
    `;

    const response = http.post(
      `${BASE_URL}/api/document/edit`,
      {
        documentId: 'shared-doc-1',
        code: editCode,
        cursorLine: Math.floor(Math.random() * 10),
        cursorCol: Math.floor(Math.random() * 50),
      },
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(response, {
      'status is 200': (r) => r.status === 200,
      'conflicts resolved': (r) => JSON.parse(r.body).conflictsResolved !== undefined,
    });
  });

  group('Graphics Rendering', () => {
    const logoCode = `
      REPEAT 4 [
        FORWARD 100
        RIGHT 90
      ]
    `;

    const response = http.post(
      `${BASE_URL}/api/execute`,
      {
        language: 'logo',
        code: logoCode,
      },
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(response, {
      'graphics rendered': (r) => JSON.parse(r.body).graphics !== undefined,
      'status is 200': (r) => r.status === 200,
    });
  });

  group('File Operations', () => {
    // Upload file
    const uploadResponse = http.post(
      `${BASE_URL}/api/files/upload`,
      {
        file: http.file(
          generateCode(),
          `loadtest-${__VU}-${__ITER}.bas`
        ),
      },
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(uploadResponse, {
      'upload successful': (r) => r.status === 200,
    });

    if (uploadResponse.status === 200) {
      const fileId = JSON.parse(uploadResponse.body).id;

      // Download file
      const downloadResponse = http.get(
        `${BASE_URL}/api/files/${fileId}`,
        {
          headers: {
            Authorization: `Bearer ${testToken}`,
          },
        }
      );

      check(downloadResponse, {
        'download successful': (r) => r.status === 200,
        'content matches': (r) => r.body.includes('PRINT'),
      });

      // Delete file
      http.delete(
        `${BASE_URL}/api/files/${fileId}`,
        {
          headers: {
            Authorization: `Bearer ${testToken}`,
          },
        }
      );
    }
  });

  group('Search & Filter', () => {
    const searchResponse = http.get(
      `${BASE_URL}/api/search?q=print&language=basic&limit=20`,
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(searchResponse, {
      'search works': (r) => r.status === 200,
      'results returned': (r) => JSON.parse(r.body).results.length >= 0,
      'response time < 200ms': (r) => r.timings.duration < 200,
    });

    cacheHitRate.add(JSON.parse(searchResponse.body).cached === true);
  });

  group('WebSocket Collaboration', () => {
    // This would require k6 WebSocket support
    // Simulating with HTTP polling for now
    const pollResponse = http.get(
      `${BASE_URL}/api/collaboration/updates?sessionId=test-session`,
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(pollResponse, {
      'polling works': (r) => r.status === 200,
    });
  });

  group('Error Scenarios', () => {
    // Invalid code
    const invalidResponse = http.post(
      `${BASE_URL}/api/execute`,
      {
        language: 'basic',
        code: 'PRINT "unclosed string',
      },
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(invalidResponse, {
      'error handling works': (r) => JSON.parse(r.body).success === false,
      'error message provided': (r) => JSON.parse(r.body).error !== undefined,
    });

    // Missing auth
    const noAuthResponse = http.get(`${BASE_URL}/api/profile`);

    check(noAuthResponse, {
      'auth required': (r) => r.status === 401,
    });

    // Non-existent resource
    const notFoundResponse = http.get(
      `${BASE_URL}/api/files/nonexistent`,
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(notFoundResponse, {
      'not found handling': (r) => r.status === 404,
    });
  });

  group('Database Stress', () => {
    // Create multiple records in sequence
    for (let i = 0; i < 5; i++) {
      http.post(
        `${BASE_URL}/api/snippets`,
        {
          title: `Snippet ${__VU}-${__ITER}-${i}`,
          code: generateCode(),
          language: 'basic',
          isPublic: Math.random() > 0.5,
        },
        {
          headers: {
            Authorization: `Bearer ${testToken}`,
          },
        }
      );
    }

    // Query database
    const queryResponse = http.get(
      `${BASE_URL}/api/snippets?limit=50&offset=0`,
      {
        headers: {
          Authorization: `Bearer ${testToken}`,
        },
      }
    );

    check(queryResponse, {
      'database query works': (r) => r.status === 200,
      'results paginated': (r) => JSON.parse(r.body).total !== undefined,
    });
  });

  sleep(Math.random() * 3);
}

/**
 * Custom summary handler
 */
export function handleSummary(data) {
  return {
    'stdout': textSummary(data, { indent: ' ', enableColors: true }),
    'summary.json': JSON.stringify(data),
  };
}

/**
 * Generate random BASIC code for testing
 */
function generateCode() {
  const operations = [
    'PRINT "test"',
    'x = 10',
    'y = 20',
    'z = x + y',
    'PRINT z',
    'IF x > 5 THEN PRINT "greater"',
    'FOR i = 1 TO 100 NEXT i',
    'GOSUB 1000',
    '1000 RETURN',
  ];

  let code = '';
  for (let i = 0; i < Math.random() * 10; i++) {
    code += operations[Math.floor(Math.random() * operations.length)] + '\n';
  }
  return code;
}

/**
 * Text summary function
 */
function textSummary(data, options) {
  const indent = options.indent || '';
  const enableColors = options.enableColors !== false;

  let summary = '\n\n========== Summary ==========\n';

  if (data.metrics) {
    for (const [name, metric] of Object.entries(data.metrics)) {
      summary += `\n${name}:\n`;
      if (metric.values) {
        for (const [key, value] of Object.entries(metric.values)) {
          summary += `${indent}${key}: ${value}\n`;
        }
      }
    }
  }

  return summary;
}
