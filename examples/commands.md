Commands
================

| Pattern               | Description                                                  | Sample Hits                                                                                                                                                                      |
|:----------------------|:-------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `"cat`\|`dog"`        | ‘cat’ or ‘dog’                                               | <b><font color="red">cat</font></b> bird <b><font color="red">dog</font></b></br>lizard giraffe</br>monkey <b><font color="red">dog</font></b>                                   |
| `"^James"`            | ‘James’ at start of text                                     | <b><font color="red">James</font></b> is here</br>I owe James 40 bucks                                                                                                           |
| `"Mary$"`             | ‘Mary’ at end of text                                        | It was <b><font color="red">Mary</font></b></br>Mary went there                                                                                                                  |
| `"hi!*"`              | ‘hi’ followed by zero or more ‘!’                            | <b><font color="red">hi</font></b> there!</br><b><font color="red">hi!!!</font></b></br>hello! <b><font color="red">hi!</font></b>                                               |
| `"a+"`                | ‘a’ one or more times                                        | <b><font color="red">a</font></b> b c <b><font color="red">aa</font></b> bb cc</br>ccc <b><font color="red">aaa</font></b> bbb eeeee <b><font color="red">aaaaa</font></b>       |
| `"a{3,4}"`            | ‘a’ three to four times                                      | a b c aa bb cc</br>ccc <b><font color="red">aaa</font></b> bbb eeeee <b><font color="red">aaaa</font></b>a                                                                       |
| `"cars?"`             | ‘car’ followed by an optional ‘s’                            | what <b><font color="red">car</font></b> do you own?</br>how many <b><font color="red">cars</font></b>?                                                                          |
| `"bear."`             | ‘bear’ followed by any character                             | one <b><font color="red">bear </font></b>two <b><font color="red">bears</font></b></br><b><font color="red">bear!</font></b> teddy bear                                          |
| `"Captain [A-Za-z]+"` | ‘Captain’ followed by any single word                        | <b><font color="red">Captain America</font></b>!</br>Call me Captain</br><b><font color="red">Captain Phasma</font></b>.                                                         |
| `"(?<=no )diarrhea"`  | ‘diarrhea’ preceded by ‘no’</br>(positive look behind)       | has diarrhea</br>no <b><font color="red">diarrhea</font></b></br>tons of diarrhea                                                                                                |
| `"(?<!no )diarrhea"`  | ‘diarrhea’ *not* preceded by ‘no’</br>(negative look behind) | has <b><font color="red">diarrhea</font></b></br>no diarrhea</br>tons of <b><font color="red">diarrhea</font></b>                                                                |
| `"good((?= night))"`  | ‘good’ followed by ’ night’</br>(positive look ahead)        | say <b><font color="red">good</font></b> night</br>the good and the bad                                                                                                          |
| `"good(?! night)"`    | ‘good’ *not* followed by ’ night’</br>(negative look ahead)  | say good night</br>the <b><font color="red">good</font></b> and the bad                                                                                                          |
| `"\\bcat\\b"`         | ‘cat’ not part of any longer word                            | my <b><font color="red">cat</font></b> and dog</br>catatonic bobcat</br>my <b><font color="red">cat</font></b>’s toy</br>i love cats</br><b><font color="red">cat</font></b>-nap |
| `"cde"`               | asdf3                                                        | ab<b><font color="red">cde</font></b></br>wxyz                                                                                                                                   |

``` r
git_color <- function(text,color) {
  str_glue("<span style=\"color: {color};\">{text}</span>")
}



join_show <- function(x,y,by=NULL) {
  if (is.null(by)) {
    by <- intersect(names(x),names(y))
  }
  if (is.null(names(by))) {
    by.x <- by
    by.y <- by
  } else {
    by.x <- names(by)
    by.y <- by
  }
  x2 <- x %>% mutate(across(-c(!!!syms(by.x)),~git_color(.,"red"))) %>% rename_with(~git_color(.,"red"),.cols=-c(!!!syms(by.x)))
  y2 <- y %>% mutate(across(-c(!!!syms(by.y)),~git_color(.,"blue"))) %>% rename_with(~git_color(.,"blue"),.cols=-c(!!!syms(by.y)))
  left <- left_join(x2,y2,by=by)
  inner <- inner_join(x2,y2,by=by)
  right <- right_join(x2,y2,by=by)
  full <- full_join(x2,y2,by=by)
  list(x=x2,y=y2,left=left,right=right,inner=inner,full=full)
}


join_show_rows <- function(x,y,by=NULL) {
  if (is.null(by)) {
    by <- intersect(names(x),names(y))
  }
  if (is.null(names(by))) {
    by.x <- by
    by.y <- by
  } else {
    by.x <- names(by)
    by.y <- by
  }
  values <- full_join(x,y,by=by) %>% select(!!!syms(by.x)) %>% distinct() %>% mutate(.color=scales::col_factor("RdYlBu",domain=NULL)(row_number()))
  x2 <- x %>% left_join(values,by=by.x) %>% mutate(across(-.color,~map2_chr(.,.color,~git_color(.x,.y)))) %>% select(-.color)
  y2 <- y %>% left_join(values,by=setNames(by.x,by.y)) %>% mutate(across(-.color,~map2_chr(.,.color,~git_color(.x,.y)))) %>% select(-.color)

  left <- left_join(x2,y2,by=by)
  inner <- inner_join(x2,y2,by=by)
  right <- right_join(x2,y2,by=by)
  full <- full_join(x2,y2,by=by)
  list(x=x2,y=y2,left=left,right=right,inner=inner,full=full)
}
```

``` r
library(yingtools2)
t <- join_show_rows(band_members,band_instruments)



knitr::kables(list(
  kable(t$x,caption="x"),
  kable(t$y,caption="y")
))
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| name                                      | band                                         |
|:------------------------------------------|:---------------------------------------------|
| <span style="color: #D7191C;">Mick</span> | <span style="color: #D7191C;">Stones</span>  |
| <span style="color: #FDAE61;">John</span> | <span style="color: #FDAE61;">Beatles</span> |
| <span style="color: #ABD9E9;">Paul</span> | <span style="color: #ABD9E9;">Beatles</span> |

x

</td>
<td>

| name                                       | plays                                       |
|:-------------------------------------------|:--------------------------------------------|
| <span style="color: #FDAE61;">John</span>  | <span style="color: #FDAE61;">guitar</span> |
| <span style="color: #ABD9E9;">Paul</span>  | <span style="color: #ABD9E9;">bass</span>   |
| <span style="color: #2C7BB6;">Keith</span> | <span style="color: #2C7BB6;">guitar</span> |

y

</td>
</tr>
</tbody>
</table>

``` r
kable(t$left,caption="left_join")
```

| name                                      | band                                         | plays                                       |
|:------------------------------------------|:---------------------------------------------|:--------------------------------------------|
| <span style="color: #D7191C;">Mick</span> | <span style="color: #D7191C;">Stones</span>  | NA                                          |
| <span style="color: #FDAE61;">John</span> | <span style="color: #FDAE61;">Beatles</span> | <span style="color: #FDAE61;">guitar</span> |
| <span style="color: #ABD9E9;">Paul</span> | <span style="color: #ABD9E9;">Beatles</span> | <span style="color: #ABD9E9;">bass</span>   |

left\_join

``` r
kable(t$right,caption="right_join")
```

| name                                       | band                                         | plays                                       |
|:-------------------------------------------|:---------------------------------------------|:--------------------------------------------|
| <span style="color: #FDAE61;">John</span>  | <span style="color: #FDAE61;">Beatles</span> | <span style="color: #FDAE61;">guitar</span> |
| <span style="color: #ABD9E9;">Paul</span>  | <span style="color: #ABD9E9;">Beatles</span> | <span style="color: #ABD9E9;">bass</span>   |
| <span style="color: #2C7BB6;">Keith</span> | NA                                           | <span style="color: #2C7BB6;">guitar</span> |

right\_join

``` r
kable(t$inner,caption="inner_join")
```

| name                                      | band                                         | plays                                       |
|:------------------------------------------|:---------------------------------------------|:--------------------------------------------|
| <span style="color: #FDAE61;">John</span> | <span style="color: #FDAE61;">Beatles</span> | <span style="color: #FDAE61;">guitar</span> |
| <span style="color: #ABD9E9;">Paul</span> | <span style="color: #ABD9E9;">Beatles</span> | <span style="color: #ABD9E9;">bass</span>   |

inner\_join

``` r
kable(t$full,caption="full_join")
```

| name                                       | band                                         | plays                                       |
|:-------------------------------------------|:---------------------------------------------|:--------------------------------------------|
| <span style="color: #D7191C;">Mick</span>  | <span style="color: #D7191C;">Stones</span>  | NA                                          |
| <span style="color: #FDAE61;">John</span>  | <span style="color: #FDAE61;">Beatles</span> | <span style="color: #FDAE61;">guitar</span> |
| <span style="color: #ABD9E9;">Paul</span>  | <span style="color: #ABD9E9;">Beatles</span> | <span style="color: #ABD9E9;">bass</span>   |
| <span style="color: #2C7BB6;">Keith</span> | NA                                           | <span style="color: #2C7BB6;">guitar</span> |

full\_join

``` r
t <- join_show(band_members,band_instruments)
# t <- join_show(cid.patients,cid.cdiff)

knitr::kables(list(
  kable(t$x,caption="x"),
  kable(t$y,caption="y")
))
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

| name | <span style="color: red;">band</span>    |
|:-----|:-----------------------------------------|
| Mick | <span style="color: red;">Stones</span>  |
| John | <span style="color: red;">Beatles</span> |
| Paul | <span style="color: red;">Beatles</span> |

x

</td>
<td>

| name  | <span style="color: blue;">plays</span>  |
|:------|:-----------------------------------------|
| John  | <span style="color: blue;">guitar</span> |
| Paul  | <span style="color: blue;">bass</span>   |
| Keith | <span style="color: blue;">guitar</span> |

y

</td>
</tr>
</tbody>
</table>

``` r
kable(t$left,caption="left_join")
```

| name | <span style="color: red;">band</span>    | <span style="color: blue;">plays</span>  |
|:-----|:-----------------------------------------|:-----------------------------------------|
| Mick | <span style="color: red;">Stones</span>  | NA                                       |
| John | <span style="color: red;">Beatles</span> | <span style="color: blue;">guitar</span> |
| Paul | <span style="color: red;">Beatles</span> | <span style="color: blue;">bass</span>   |

left\_join

``` r
kable(t$right,caption="right_join")
```

| name  | <span style="color: red;">band</span>    | <span style="color: blue;">plays</span>  |
|:------|:-----------------------------------------|:-----------------------------------------|
| John  | <span style="color: red;">Beatles</span> | <span style="color: blue;">guitar</span> |
| Paul  | <span style="color: red;">Beatles</span> | <span style="color: blue;">bass</span>   |
| Keith | NA                                       | <span style="color: blue;">guitar</span> |

right\_join

``` r
kable(t$inner,caption="inner_join")
```

| name | <span style="color: red;">band</span>    | <span style="color: blue;">plays</span>  |
|:-----|:-----------------------------------------|:-----------------------------------------|
| John | <span style="color: red;">Beatles</span> | <span style="color: blue;">guitar</span> |
| Paul | <span style="color: red;">Beatles</span> | <span style="color: blue;">bass</span>   |

inner\_join

``` r
kable(t$full,caption="full_join")
```

| name  | <span style="color: red;">band</span>    | <span style="color: blue;">plays</span>  |
|:------|:-----------------------------------------|:-----------------------------------------|
| Mick  | <span style="color: red;">Stones</span>  | NA                                       |
| John  | <span style="color: red;">Beatles</span> | <span style="color: blue;">guitar</span> |
| Paul  | <span style="color: red;">Beatles</span> | <span style="color: blue;">bass</span>   |
| Keith | NA                                       | <span style="color: blue;">guitar</span> |

full\_join
