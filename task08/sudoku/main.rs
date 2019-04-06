/**
 * Это основной файл с домашним заданием по языку Rust
 * Кроме него есть еще модуль field.rs, лежащий в этой же папке
 * Что бы собрать программу, напишите в терминале cargo build
 * Что бы почистить за собой -- cargo clean
 *
 * Что бы вам было легче оринетироваться мы постарались 
 * прокомментировать весь код, насколько это возможно
 */

// Модули, которые нам понадобятся -- ввод\вывод и работа с памятью
use std::io;
use std::mem;

// Намек компилятору, что мы хотим использовать наш модуль field.rs
mod field;

// И, собственно, то, что нам оттуда нужно
use field::{N, Field, parse_field};
use field::Cell::*;

/**
 * Эта функция пытается "расширить" данное ей игровое поле
 * Под расширением мы понимаем просто добавление числа на поле
 * и попытку найти решение. Она принимает на вход три параметра:
 * 1. f: &mut Field -- мутабельную ссылку на поле(поле приходится менять и передавать туда-сюда)
 * 2. solved_cb : impl Fn(&mut Field) -> T -- замыкание, которые вызывается если мы нашли решение
 * 3. next_step_cb: impl Fn(&mut Field) -> Option<T> -- замыкание, которые вызывается, когда мы не 
 * до конца заполнили поле, и над ним нужно сделать следующий шаг
 * 
 * Замыкание -- это просто анонимная функция, котрая может что-либо захватить
 * (переменные из объемлющей области видимости). Здесь мы просим, что бы они захватывали
 * все по ссылке(Fn)
 *
 * В качестве результата она возвращает Option<T> -- это тип, который говорит нам о том, что
 * вычисление могло завершиться неудачей и результат не известен. По такому поводу у него два 
 * констурктора с разной семантикой:
 * 1. Some(x) -- вычисление завершилось успешно, x -- его результат
 * 2. None -- вычисление завершилось неудачно, результата нет, увы
 */
fn try_extend_field<T>(f: &mut Field, solved_cb: impl Fn(&mut Field) -> T, next_step_cb: impl Fn(&mut Field) -> Option<T>) -> Option<T> {
    // Проверяем простые случаи:
    // Поле противоречиво -- решения нет
    if f.contradictory() {
        return None;
    }
    // Все заполнено, решение есть, надо вызвать наш колбэк-второй параметр
    // и запаковать результат в Some
    if f.full() {
        return Some(solved_cb(f));
    }
    // Пошли перебирать клетки
    for row in 0..N {
        for col in 0..N {
            // Нашли пустую -- начинем перебирать все, что можно туда поставить
            if f.0[row][col] == Empty {
                for d in 1..=N {
                    f.0[row][col] = Digit(d);
                    // поставили -- вызвали колбэк(третий параметр)
                    // Здесь мы смотрим, если он вернул Some(x) -- значит мы нашли решение
                    // Надо его вернуть и дело с концом
                    // Иначе -- перебираем дальше
                    if let Some(x) = next_step_cb(f) {
                        return Some(x);
                    }
                    // И возвращаем все как было
                    f.0[row][col] = Empty;
                }
                return None;
            }
        }
    }
    // Эта функция валит программу с ошибкой и пишет соответствующее сообщение
    panic!("Field should've been non-full");
}

/**
 * Эта функция и запускает наши вычисления, вызывая предущую
 */
fn find_solution(f: &mut Field) -> Option<Field> {
    try_extend_field(f,
      |f_solved| f_solved.clone(),
      find_solution
    )
}

// Точка входа в нашу программу
fn main() -> io::Result<()> {
    use std::io::BufRead;

    let stdin = std::io::stdin();
    // Читаем поле из stdin, заводим под него мутабельную переменную
    let mut field = parse_field(stdin.lock().lines().map(|l| l.unwrap()));
    // stdin перестал быть нужен, избавляемся от него
    mem::drop(stdin);

    println!("{:?}", field);
    // Запускаем поиск решения, если оно есть, то печатаем его,
    // если нет, пишем "No solution" -- за это отвечает вызов .expect()
    let solution = find_solution(&mut field).expect("No solution");
    println!("{:?}", solution);
    // Нам надо вернуть что-то типа Result<()>, вернем вот это значение
    Ok(())
}