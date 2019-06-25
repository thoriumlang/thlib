/*
 * Copyright 2019 Christophe Pollet
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.thoriumlang.thlib.pattern;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

public class ThPatternMatcherTest {
    @Test
    void match() {
        ThPatternMatcher<String> matcher = new ThPatternMatcher<>(
                Arrays.asList(
                        new ThPattern<>(Integer.class, v -> String.format("Integer: %d", v)),
                        new ThPattern<>(String.class, v -> String.format("String: %s", v))
                )
        );

        Assertions.assertThat(matcher.eval(() -> "string"))
                .hasValue("String: string");
        Assertions.assertThat(matcher.eval(() -> 1))
                .hasValue("Integer: 1");
        Assertions.assertThat(matcher.eval(() -> true))
                .isNotPresent();
    }

    @Test
    void match2() {
        ThPatternMatcher<String> matcher = new ThPatternMatcher<String>()
                .when(Integer.class, v -> String.format("Integer: %d", v))
                .when(String.class, v -> String.format("String: %s", v));

        Assertions.assertThat(matcher.eval(() -> "string"))
                .hasValue("String: string");
        Assertions.assertThat(matcher.eval(() -> 1))
                .hasValue("Integer: 1");
        Assertions.assertThat(matcher.eval(() -> true))
                .isNotPresent();

        matcher = matcher.otherwise(v -> String.format("Other: %s", v));
        Assertions.assertThat(matcher.eval(() -> true))
                .hasValue("Other: true");

    }
}
