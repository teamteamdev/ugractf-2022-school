# Лестница: Write-up

Мы наблюдаем вход ~~в подъезд~~ конечно же, на лестницу где-то в Петербурге. Дверь такая хорошая, добротная, явно стоит там не менее 60 лет. (После 1960, при Хрущёве и далее, таких уже не делали.)

Немного поискав, можно обнаружить сайт [dominfospb.ru](https://dominfospb.ru/). Мы знаем из фотографии, что ~~подъезд~~ лестница находится в доме 33. [Поиск на сайте по строке «33» в адресе](https://dominfospb.ru/search?adres=33) даёт порядка 350 домов. В них есть немного лишних (дома с номерами 133, например).

Соберём для каждого такого дома страничку с информацией. Для фильтрации будем использовать минимальную и максимальную этажность (мы знаем, что этажей на нашей лестнице пять), а также год постройки (чтобы отсечь массово строившиеся после 1960 пятиэтажки).

Можем преобразовывать страницы в текст и вычленять данные таким регулярным выражением: `Адрес дома:\xa0(.*?)Год постройки:\xa0(.*?).*Наибольшее количество этажей, ед.:\xa0(.*?)\s*Наименьшее количество этажей, ед.:\xa0(\d*)`

До фильтрации:

```python
...
[('г. Санкт-Петербург, ул. Бестужевская, д. 33, к. 1, лит. А', '1972', '14', '14')]
[('г. Санкт-Петербург, пр-кт Лермонтовский, д. 33, лит. А', '1879', '5', '4')]
...
```

После фильтрации:

```
г. Санкт-Петербург, пр-кт Большой П.С., д. 33а, лит. А
г. Санкт-Петербург, пр-кт Большой Сампсониевский, д. 33/1, лит. А
г. Санкт-Петербург, пр-кт Костромской, д. 33 литер А
г. Санкт-Петербург, пр-кт Лиговский, д. 33, к. 35
г. Санкт-Петербург, пр-кт Малый В.О., д. 33, лит. А
г. Санкт-Петербург, пр-кт Обуховской Обороны, д. 33, к. 1
г. Санкт-Петербург, пр-кт Обуховской Обороны, д. 33, к. 2
г. Санкт-Петербург, ул. 6-я Советская, д. 33
г. Санкт-Петербург, ул. 8-я Советская, д. 33
г. Санкт-Петербург, ул. Блохина, д. 33, лит. А
г. Санкт-Петербург, ул. Большая Пороховская, д. 33, лит.А
г. Санкт-Петербург, ул. Ждановская, д. 33А
г. Санкт-Петербург, ул. Жуковского, д. 33, стр. А
г. Санкт-Петербург, ул. Зайцева, д. 33, лит. А
г. Санкт-Петербург, ул. Итальянская, д. 33, лит. А
г. Санкт-Петербург, ул. Можайская, д. 33, к. А
г. Санкт-Петербург, ул. Некрасова, д. 33
г. Санкт-Петербург, ул. Профессора Попова, д. 33, лит. А
г. Санкт-Петербург, ул. Разъезжая, д. 33
г. Санкт-Петербург, ул. Розенштейна, д. 33, лит. А
г. Санкт-Петербург, ул. Рылеева, д. 33
г. Санкт-Петербург, ул. Седова, д. 33
г. Санкт-Петербург, ул. Съезжинская, д. 33, лит. А
```

Ещё можно заметить торчащее на указателе «ULITSA» и отбросить проспекты. Остаётся уже вполне обозримое количество домов. Можно было бы обойти все дома, например, на Панорамах, но это достаточно трудоёмко.

У дома есть одна очень редкая особенность: квартира номер 4 находится аж на пятом этаже. Обычно на этаж каждой лестницы приходится всё-таки хотя бы по две квартиры (чаще всего три-четыре — а во многих домах и больше). Здесь же на этаж приходится по одной квартире, а на первом этаже ничего нет вовсе (хотя в прежние времена всё-таки было — со странным номером 19).

Ещё один источник информации об объектах недвижимости — Росреестр. Там, в частности, можно узнать по номеру квартиры, на каком она этаже (а ещё много всего интересного). Официальный поиск Росреестра — «[Справочная информация по объектам недвижимости в режиме online
](https://lk.rosreestr.ru/eservices/real-estate-objects-online)» — позволяет после ввода капчи и адреса в относительно свободной форме (лишь бы поиск отработал) открыть информацию о конкретной квартире.

Перебором находим, что такая квартира есть в доме **улица Жуковского, 33** (в Росреестре почему-то записан как **33 литер А**). Смотрим на Яндексе координаты входа: **59.935972, 30.356478**. Можно с Панорам подтвердить, что табличка действительно та (а номер дома успел переехать с того места, где он оказался так кстати).

Не забываем, что нас просят указать долготу относительно Пулковского меридиана. Согласно Википедии, это 30.326053° восточной долготы — это число надо вычесть из долготы относительно общепринятого в современности нулевого меридиана. Так что в форму надо вводить ответ **59.935972, 0.030425**.

Флаг: **ugra_and_she_is_buying_one_to_heaven_62a92f831b14**