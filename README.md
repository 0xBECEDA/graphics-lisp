# graphics-lisp

Проект представляет собой зачаток графической библиотеки для отрисовки
графиков на базе библиотеки zpng.
На данный момент можно:
- заполнять график фоновым цветом
- генерировать неподписанные оси и деления на них
- генерировать точки на графике и соеднять их

Что планиурется добавить:
- масштабируемость графиков
- собственный масштабируемый шрифт для подписывния осей и т.д.
- возможность соединять любые две точки на графике - это требует
определенных ухищрений с алгоритмом Брезенхема и машстабируемостью графиков

В файле doc.org вы найдете документацию по проекту. В файле
graphics.lisp - весь код, написанный на лиспе.
