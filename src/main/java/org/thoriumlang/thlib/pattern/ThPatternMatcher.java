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

import org.thoriumlang.thlib.types.ThLazy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class ThPatternMatcher<R> {
    private final List<ThPattern<R>> patterns;

    public ThPatternMatcher(List<ThPattern<R>> patterns) {
        this.patterns = patterns;
    }

    public ThPatternMatcher() {
        this(Collections.emptyList());
    }

    public ThPatternMatcher<R> when(Class<?> clazz, Function<Object, R> f) {
        ArrayList<ThPattern<R>> list = new ArrayList<>(patterns);
        list.add(new ThClassPattern<>(clazz, f));
        return new ThPatternMatcher<>(list);
    }

    public ThPatternMatcher<R> when(Object o, Function<Object, R> f) {
        ArrayList<ThPattern<R>> list = new ArrayList<>(patterns);
        list.add(new ThObjectPattern<>(o, f));
        return new ThPatternMatcher<>(list);
    }

    public ThPatternMatcher<R> otherwise(Function<Object, R> f) {
        ArrayList<ThPattern<R>> list = new ArrayList<>(patterns);
        list.add(new ThTruePattern<>(f));
        return new ThPatternMatcher<>(list);
    }

    public Optional<R> eval(ThLazy<Object> value) {
        Object v = value.eval();
        return patterns.stream()
                .filter(p -> p.matches(v))
                .findFirst()
                .orElse(new ThTruePattern<>(x -> null))
                .eval(v);
    }
}
