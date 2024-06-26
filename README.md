# Diabetes data
Данные  были получены из лаборатории городской больницы Ирака и Специализированного центра эндокринологии и диабета, клинической больницы Аль-Кинди. Данные состоят из медицинской информации и лабораторных анализов. В датасете 1000 записей о пациентах.

- Patient ID (Идентификационный номер пациента), 
- No_Pation (Номер пациента), 
- Gender (Пол), 
- AGE (Возраст в годах),
- Urea (Уровень мочевины в крови), 
- Cr (Уровень креатинина), 
- HbA1c (Уровень гемоглобина A1c. Гликированный гемоглобин, отражающий среднее содержание сахара в крови за длительный период (от трёх до четырёх месяцев),
- Chol (Общий уровень холестерина),
- TG (Уровень триглециридов в крови пациента),
- HDL (Уровень липопротеинов высокой плотности),
- LDL (Уровень липопротеинов низкой плотности),
- VLDL (Уровень липопротеинов очень низкой плотности),
- BMI (Индекс массы тела),
- CLASS (Класс пациента: Y - есть диабет, N - нет диабета, P - предиабет)

# Задания

### Подготовка данных:

- Проведите предобработку данных, убедитесь в том, что  с данными все хорошо (нет неодинаково записанных данных, например в Gender могут быть следующие записи “m” и “M” в CLASS “Y “ (с пробелом) и “Y”
- Проведите исследовательский анализ данных - посмотрите какие типы данных представлены в таблице, убедитесь, что нет выбросов, пропущенных значений, дубликатов.
- Посчитайте сколько пациентов разного пола и класса представлено в датасете

### Описательная статистика:

- Рассчитайте средние значения, медиану, моду, минимум и максимум для каждого числового столбца (AGE, Urea, Cr, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI) в зависимости от класса и пола, а также по всему (несгруппированному) датасету. 
Посчитайте различие между показателями, рассчитанных по всему датасету и показателями каждого класса
- Необходимо построить гистограммы для числовых столбцов, чтобы увидеть их распределение в зависимости 
1.от пола 
2.каждого класса.

### Корреляционный анализ:

- Необходимо рассчитать корреляции между числовыми переменными (например, между Urea, Cr, HbA1c, Chol и т. д.).

- Построить матрицу корреляций и визуализировать её.

### Анализ распределения:

- Проверить нормальность распределения числовых переменных (например, с помощью теста Шапиро-Уилка).

- Построить QQ-графики для числовых переменных.

### Группировка и сравнение:

- Применить t-тест или анализ дисперсии (ANOVA) для проверки статистической значимости различий между разными группами  (например, между разными значениями CLASS или Gender).

- Проверьте, зависит ли от пола класс диабета
- Проанализируйте другие столбцы, выделите наиболее важный признак, который может влиять на возникновение сахарного диабета1-х

### Регрессионный анализ:

- Построить модель регрессии для предсказания одной из переменных на основе других.

- Рассчитать коэффициенты регрессии и интерпретировать их.

### Кластерный анализ:

- Применить метод кластерного анализа для выявления групп схожих наблюдений на основе значений переменных.

- Визуализировать результаты кластеризации.

### Вывод:

Сделайте вывод по полученным данным, сохраните блокнот в формате html, пришлите блокнот и html-файл на проверку


# Структура презентации проекта

- Использованные данные [скриншоты таблички]
- Набор задач
- Препроцессные данные [скриншоты таблички]
- Итоговые переменные для решения задач 
[описательные статистики и графики для исследования полученных переменных]
- Примененные статистические (если дошли до этого этапа)
- Выводы, сделанные на основании графиков и/или статистических тестов

# Что должно быть в презентации:

- Данные
- Таблица с основной информацией датасета
- Какую предобработку провели, какие интересные данные/выбросы были в датасете
- Выводы из исследовательского анализа данных
- Выводы из статистического анализа данных в виде графиков и устное объяснение к ним
- Гипотезы и выводы
- Общий вывод
