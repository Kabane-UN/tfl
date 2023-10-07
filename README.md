# TFL 2023
### Здесь лежат лабы по ТФЯ Кабанова Андрей Юрьевича ИУ9-52Б
## Оглавление
* [Первая лаба](#Первая-лаба)
* [Вторая лаба](#Вторая-лаба)
## Первая лаба (вариант 3)

Лаба находится в папке lab1
### Структура
* main.scala исходник кода
* main.jar скомпилированная программа
* input.txt файл из которого читаются входные данные (пример входных данных есть)
* SMTPattern.txt файл балванка
* script.py скрипт запускающий программу и SMTsolver
### Детали и инструкция
* используется солвер z3
* написано и скомпилировано на версии Scala 3.3.0
* файл скрипта на питоне запускается так
1. Использовать jar файл
```shell
python3 script.py -jar
```
2. Не знаю зачем, но можно использовать исходный файл на прямую
```shell
python3 script.py -scala
```
`Это будет на много дольше`
* понятное дело нужна scala 3.3.0

Из-за специфики реализации программа не справляется с безумными вложеностями,
но с ними и z3 не справляется, поэтому как-то так

На этом все, пожалуйста не судите строго)
## Вторая лаба (вариант 1)

Лаба находится в папке lab1
Авторы:
* Кабанов Андрей
* Федоров Владислав
### Структура
* main.scala исходник кода тестирования
* main.jar скомпилированная программа тестирования
* script.py скрипт запускающий основную программу и тесты
* regexFsm.txt файлы балванка хранящий автомат регулярок
* regex.txt файл куда тастер записывает сгенерированные регулярки
* fsm.txt файл откуда тестер берет автоматы и академические регулярки
### Детали реализации
#### Часть Андрея
При написании генератора регулярок я решил выпендрится, и решил написать автомат, который генерирует регулярки. Проблема в том, что язык регулярок не регулярен. Из-за всего того-же присловутого языка Dyck. Но его можно сделать регулярным, получившийся язык называется языком *сжатых* регулярок (нужно просто разрешить удалять любой превикс из ((( и любой постфикс из )))). Теперь нужно написать соответствующую регулярку и автомат. На регулярку я потратил день, так-как надо было запретить пустые скобки с пустые аргументы. Когда с регуляркой было покончено, один интересный сайтик построил мне минимальный DFA.
Далее все просто (нет). Строим програму, которая выбирает финальное состояние и далее рандомно гуляет по обратным путям. НО! Нужно следить за звездной высотой, помог стек, хранящий текущую вложенность, и откаты назад. Теперь веселее, нужно учитывать ограничение на длину, тут я строю при помощи BFS минимальные обратные пути из любого состояния в начальное. Далее при каждой итерации смотрю сколько путей (при которых не привышается максимальная длина) есть из состояния в которое я хочу перейти в начальное. Далее рандомно выбираю любое. Благодаря проверки на каждой итерации, мы гарантированно имеет хотябы один путь. Естественно, что изначально есть проверка, что если при текущей максимальной длине, ни из одного финального состояния нет пути в начальное, то мы кидаем ошибку генерации.
Со второй частью, а именно с генерацией слов из автоматов, было очень легко. Используется тот-же самый код (почти). Просто еще одна похожая функция которая уже генерирует слова не из языка (при первой итерации берем любое не финальное). Тут нужна ловушка, но ее добавить просто. Благодаря наработкам из первой части, получилось ограничить максимальнуб длину тестирующих слов, хоть этого и не требовалось в ТЗ. Далее все тривиально.

Также я на всякий случай включил в тестирующий модуль самотесты. Они проверяют, правда ли сгенерированное слово принимается автоматом и наоборот (насамом деле я просто написал алгоритм проверки вхождения слова в автомат и незнал куда его присобачить) 

Также спицифика реализации такова, что если автомату не удасться сгенерировать слово принадлежащее или не принадлежащее языку (либо ограничение длины слишком мало, либо просто язык является всеобъемлющим), то соответствующий тест пропускается и никак не влияет на ответ.

Как уже понятно, я решил немного отойти от ТЗ и при генерации регулярок учитывать не максимальное количество букв, а максимальную длину регулярки. Мне кажется это более правильным и сложным.
### Инструкция
* написано и скомпилировано на версии Scala 3.3.0
* скрипт на питоне запускается так
```shell
python3 script.py 3 50 4 100 50 200
```
Аргументы:

1. Мощьность алфавита (1 to 5)
2. Максимальная длина регулярки (1 to ∞)
3. Максмальная звездная высота (0 to 5)
4. Количество сгенерированных регулярок
5. Максимальная длина слов сгенерированных для Fuzz тестирования
6. Количество тестов на один автомат (делится пополам на входящие в язык и невходящие)