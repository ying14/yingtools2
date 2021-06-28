Regular Expressions
================

<table>
<thead>
<tr>
<th style="text-align:left;">
Pattern
</th>
<th style="text-align:left;">
Description
</th>
<th style="text-align:left;">
Sample Hits
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
`&quot;cat|dog&quot;`
</td>
<td style="text-align:left;">
‘cat’ or ‘dog’
</td>
<td style="text-align:left;">
&lt;span style=‘color: red;’&gt;cat&lt;/span&gt; bird &lt;span
style=‘color: red;’&gt;dog&lt;/span&gt;&lt;/br&gt;lizard
giraffe&lt;/br&gt;monkey &lt;span style=‘color:
red;’&gt;dog&lt;/span&gt;
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;^James&quot;`
</td>
<td style="text-align:left;">
‘James’ at start of text
</td>
<td style="text-align:left;">
&lt;span style=‘color: red;’&gt;James&lt;/span&gt; is here&lt;/br&gt;I
owe James 40 bucks
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;Mary$&quot;`
</td>
<td style="text-align:left;">
‘Mary’ at end of text
</td>
<td style="text-align:left;">
It was &lt;span style=‘color: red;’&gt;Mary&lt;/span&gt;&lt;/br&gt;Mary
went there
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;hi!*&quot;`
</td>
<td style="text-align:left;">
‘hi’ followed by zero or more ‘!’
</td>
<td style="text-align:left;">
&lt;span style=‘color: red;’&gt;hi&lt;/span&gt;
there!&lt;/br&gt;&lt;span style=‘color:
red;’&gt;hi!!!&lt;/span&gt;&lt;/br&gt;hello! &lt;span style=‘color:
red;’&gt;hi!&lt;/span&gt;
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;a+&quot;`
</td>
<td style="text-align:left;">
‘a’ one or more times
</td>
<td style="text-align:left;">
&lt;span style=‘color: red;’&gt;a&lt;/span&gt; b c &lt;span
style=‘color: red;’&gt;aa&lt;/span&gt; bb cc&lt;/br&gt;ccc &lt;span
style=‘color: red;’&gt;aaa&lt;/span&gt; bbb eeeee &lt;span style=‘color:
red;’&gt;aaaaa&lt;/span&gt;
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;a{3,4}&quot;`
</td>
<td style="text-align:left;">
‘a’ three to four times
</td>
<td style="text-align:left;">
a b c aa bb cc&lt;/br&gt;ccc &lt;span style=‘color:
red;’&gt;aaa&lt;/span&gt; bbb eeeee &lt;span style=‘color:
red;’&gt;aaaa&lt;/span&gt;a
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;cars?&quot;`
</td>
<td style="text-align:left;">
‘car’ followed by an optional ‘s’
</td>
<td style="text-align:left;">
what &lt;span style=‘color: red;’&gt;car&lt;/span&gt; do you
own?&lt;/br&gt;how many &lt;span style=‘color:
red;’&gt;cars&lt;/span&gt;?
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;bear.&quot;`
</td>
<td style="text-align:left;">
‘bear’ followed by any character
</td>
<td style="text-align:left;">
one &lt;span style=‘color: red;’&gt;bear &lt;/span&gt;two &lt;span
style=‘color: red;’&gt;bears&lt;/span&gt;&lt;/br&gt;&lt;span
style=‘color: red;’&gt;bear!&lt;/span&gt; teddy bear
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;Captain [A-Za-z]+&quot;`
</td>
<td style="text-align:left;">
‘Captain’ followed by any single word
</td>
<td style="text-align:left;">
&lt;span style=‘color: red;’&gt;Captain
America&lt;/span&gt;!&lt;/br&gt;Call me Captain&lt;/br&gt;&lt;span
style=‘color: red;’&gt;Captain Phasma&lt;/span&gt;.
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;(?&lt;=no )diarrhea&quot;`
</td>
<td style="text-align:left;">
‘diarrhea’ preceded by ‘no’&lt;/br&gt;(positive look behind)
</td>
<td style="text-align:left;">
has diarrhea&lt;/br&gt;no &lt;span style=‘color:
red;’&gt;diarrhea&lt;/span&gt;&lt;/br&gt;tons of diarrhea
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;(?&lt;!no )diarrhea&quot;`
</td>
<td style="text-align:left;">
‘diarrhea’ *not* preceded by ‘no’&lt;/br&gt;(negative look behind)
</td>
<td style="text-align:left;">
has &lt;span style=‘color: red;’&gt;diarrhea&lt;/span&gt;&lt;/br&gt;no
diarrhea&lt;/br&gt;tons of &lt;span style=‘color:
red;’&gt;diarrhea&lt;/span&gt;
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;good((?= night))&quot;`
</td>
<td style="text-align:left;">
‘good’ followed by ’ night’&lt;/br&gt;(positive look ahead)
</td>
<td style="text-align:left;">
say &lt;span style=‘color: red;’&gt;good&lt;/span&gt;
night&lt;/br&gt;the good and the bad
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;good(?! night)&quot;`
</td>
<td style="text-align:left;">
‘good’ *not* followed by ’ night’&lt;/br&gt;(negative look ahead)
</td>
<td style="text-align:left;">
say good night&lt;/br&gt;the &lt;span style=‘color:
red;’&gt;good&lt;/span&gt; and the bad
</td>
</tr>
<tr>
<td style="text-align:left;">
`&quot;\\bcat\\b&quot;`
</td>
<td style="text-align:left;">
‘cat’ not part of any longer word
</td>
<td style="text-align:left;">
my &lt;span style=‘color: red;’&gt;cat&lt;/span&gt; and
dog&lt;/br&gt;catatonic bobcat&lt;/br&gt;my &lt;span style=‘color:
red;’&gt;cat&lt;/span&gt;‘s toy&lt;/br&gt;i love cats&lt;/br&gt;&lt;span
style=’color: red;’&gt;cat&lt;/span&gt;-nap
</td>
</tr>
</tbody>
</table>
