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

import java.util.Optional;
import java.util.function.Function;

public class ThObjectPattern<R> implements ThPattern<R> {
    private final Object object;
    private final Function<Object, R> f;

    public ThObjectPattern(Object object, Function<Object, R> f) {
        this.object = object;
        this.f = f;
    }

    @Override
    public boolean matches(Object value) {
        return value != null && value.equals(object);
    }

    @Override
    public Optional<R> eval(Object value) {
        return Optional.ofNullable(f.apply(value));
    }
}
