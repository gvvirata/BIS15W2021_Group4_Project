---
title: "Title"
author: ""
date: "2021-02-18"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---




```r
#if there's anymore libraries we might need, include them here :)
library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(janitor)
library(here)
library(naniar)
```


```r
plant_sizes <- read_csv(here("Plant+Pollinators Data", "Flower size measurements.csv")) %>% janitor::clean_names()
plant_pollinators <- read_csv(here("Plant+Pollinators Data", "Pollinator and flower color.csv")) %>% janitor::clean_names()
```


```r
plants_merge <- merge(plant_sizes, plant_pollinators, by="species")
plants <- plants_merge %>%
  select(-mean_corolla_length_cm, -standard_deviation_corolla_length_cm, -mean_corolla_width_cm, -standard_deviation_corolla_width_cm, -length_width_ratio) %>%
  rename(color_source="source", pollinator_source="source_1") %>%
  filter(herbarium_accession_sheet!="NA" & flower_number!="NA" & corolla_length_cm!="NA" & corolla_width_throat_cm!="NA" & color!="NA" & pollinator!="NA") %>%
  mutate_all(funs(str_replace(., " or", ", "))) %>%
  mutate_all(funs(str_replace(., " and", ", "))) %>%
  mutate_all(funs(str_replace(., "/", ", "))) %>%
  mutate_all(funs(str_replace(., " to", ", "))) %>%
  separate(color, into=c("color_opt_1", "color_opt_2", "color_opt_3", "color_opt_4"), sep=", ") %>%
  mutate_all(funs(str_replace(., " primary,", ", "))) %>%
  mutate_all(funs(str_replace(., " secondary", ""))) %>%
  mutate_all(funs(str_replace(., "bees", "bee"))) %>%
  mutate_all(funs(str_replace(., "butterflies", "butterfly"))) %>%
  mutate_all(funs(str_replace(., "beeflies", "bee-fly"))) %>%
  mutate_all(funs(str_replace(., "beefly", "bee-fly"))) %>%
  na_if("autogamous") %>%
  separate(pollinator, into=c("pollinator_1", "pollinator_2", "pollinator_3"), sep=", ")
plants
```

```
                         species                herbarium_accession_sheet
1          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172802
2          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172802
3          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172801
4          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172802
5           Aliciella caespitosa          Aliciella caespitosa KSC0126229
6           Aliciella caespitosa          Aliciella caespitosa KSC0126229
7           Aliciella caespitosa          Aliciella caespitosa KSC0126229
8       Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00237643
9       Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00323745
10      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00237643
11      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00323745
12      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00237643
13      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00323745
14          Aliciella leptomeria             Aliciella leptomeria UF39347
15          Aliciella leptomeria        Aliciella leptomeria KANU00237698
16          Aliciella leptomeria           Aliciella leptomeria FLAS39347
17          Aliciella leptomeria             Aliciella leptomeria UF39347
18          Aliciella leptomeria           Aliciella leptomeria FLAS39347
19          Aliciella leptomeria        Aliciella leptomeria KANU00237698
20          Aliciella leptomeria           Aliciella leptomeria FLAS39347
21          Aliciella leptomeria             Aliciella leptomeria UF39347
22          Aliciella leptomeria        Aliciella leptomeria KANU00237698
23          Aliciella micromeria        Aliciella micromeria KANU00237702
24          Aliciella micromeria        Aliciella micromeria KANU00237702
25          Aliciella micromeria        Aliciella micromeria KANU00237702
26         Aliciella pinnatifida       Aliciella pinnatifida KANU00237715
27         Aliciella pinnatifida       Aliciella pinnatifida KANU00237715
28         Aliciella pinnatifida       Aliciella pinnatifida KANU00237717
29         Aliciella pinnatifida       Aliciella pinnatifida KANU00237715
30         Aliciella pinnatifida       Aliciella pinnatifida KANU00237717
31         Aliciella pinnatifida       Aliciella pinnatifida KANU00237717
32             Aliciella subnuda           Aliciella subnuda KANU00237798
33             Aliciella subnuda           Aliciella subnuda KANU00237796
34             Aliciella subnuda           Aliciella subnuda KANU00237795
35             Aliciella subnuda           Aliciella subnuda KANU00237795
36             Aliciella subnuda           Aliciella subnuda KANU00247797
37             Aliciella subnuda             Aliciella subnuda KANU362789
38             Aliciella subnuda           Aliciella subnuda KANU00237798
39             Aliciella subnuda           Aliciella subnuda KANU00247797
40             Aliciella subnuda             Aliciella subnuda KSC0126390
41             Aliciella subnuda           Aliciella subnuda KANU00237795
42             Aliciella subnuda           Aliciella subnuda KANU00237796
43             Aliciella subnuda             Aliciella subnuda KSC0126390
44             Aliciella subnuda           Aliciella subnuda KANU00237796
45             Aliciella subnuda             Aliciella subnuda KSC0126390
46             Aliciella subnuda           Aliciella subnuda KANU00247797
47             Aliciella subnuda             Aliciella subnuda KANU362789
48             Aliciella subnuda           Aliciella subnuda KANU00237798
49             Aliciella subnuda             Aliciella subnuda KANU362789
50       Allophyllum divaricatum       Allophyllum divaricatum KSC0124636
51       Allophyllum divaricatum       Allophyllum divaricatum KSC0124636
52       Allophyllum divaricatum       Allophyllum divaricatum KSC0124636
53         Allophyllum gilioides         Allophyllum gilioides FLAS119038
54         Allophyllum gilioides         Allophyllum gilioides FLAS124817
55         Allophyllum gilioides         Allophyllum gilioides FLAS119038
56         Allophyllum gilioides         Allophyllum gilioides KSC0126262
57         Allophyllum gilioides         Allophyllum gilioides FLAS119038
58         Allophyllum gilioides         Allophyllum gilioides KSC0126266
59         Allophyllum gilioides         Allophyllum gilioides KSC0126266
60         Allophyllum gilioides         Allophyllum gilioides KSC0126262
61         Allophyllum gilioides          Allophyllum gilioides FLAS72303
62         Allophyllum gilioides         Allophyllum gilioides KSC0126261
63         Allophyllum gilioides       Allophyllum gilioides KANU00176464
64         Allophyllum gilioides       Allophyllum gilioides KANU00176463
65         Allophyllum gilioides         Allophyllum gilioides FLAS124817
66         Allophyllum gilioides         Allophyllum gilioides KSC0126262
67         Allophyllum gilioides         Allophyllum gilioides KSC0126266
68         Allophyllum gilioides         Allophyllum gilioides FLAS124817
69         Allophyllum gilioides       Allophyllum gilioides KANU00176463
70         Allophyllum gilioides          Allophyllum gilioides FLAS72302
71         Allophyllum gilioides       Allophyllum gilioides KANU00176464
72         Allophyllum gilioides       Allophyllum gilioides KANU00176464
73         Allophyllum gilioides          Allophyllum gilioides FLAS72303
74         Allophyllum gilioides         Allophyllum gilioides KSC0126261
75         Allophyllum gilioides          Allophyllum gilioides FLAS79454
76         Allophyllum gilioides          Allophyllum gilioides FLAS72302
77         Allophyllum gilioides          Allophyllum gilioides FLAS72302
78         Allophyllum gilioides          Allophyllum gilioides FLAS79454
79         Allophyllum gilioides         Allophyllum gilioides KSC0126261
80         Allophyllum gilioides          Allophyllum gilioides FLAS72303
81         Allophyllum gilioides       Allophyllum gilioides KANU00176463
82         Allophyllum gilioides          Allophyllum gilioides FLAS79454
83        Allophyllum glutinosum        Allophyllum glutinosum KSC0126064
84        Allophyllum glutinosum       Allophyllum glutinosum KANU0176465
85        Allophyllum glutinosum       Allophyllum glutinosum KANU0176465
86        Allophyllum glutinosum        Allophyllum glutinosum KSC0126064
87        Allophyllum glutinosum       Allophyllum glutinosum KANU0176465
88        Allophyllum glutinosum        Allophyllum glutinosum KSC0126064
89     Allophyllum integrifolium      Allophyllum integrifolium MO4390038
90     Allophyllum integrifolium      Allophyllum integrifolium MO4270063
91     Allophyllum integrifolium      Allophyllum integrifolium MO4270063
92     Allophyllum integrifolium      Allophyllum integrifolium MO4390038
93     Allophyllum integrifolium      Allophyllum integrifolium MO3108259
94     Allophyllum integrifolium      Allophyllum integrifolium MO3108259
95     Allophyllum integrifolium      Allophyllum integrifolium MO4390038
96     Allophyllum integrifolium      Allophyllum integrifolium MO4270063
97     Allophyllum integrifolium      Allophyllum integrifolium MO3108259
98         Allophyllum violaceum            Allophyllum violaceum 3108257
99         Allophyllum violaceum            Allophyllum violaceum 3024210
100        Allophyllum violaceum            Allophyllum violaceum 3108257
101        Allophyllum violaceum            Allophyllum violaceum 4322220
102        Allophyllum violaceum            Allophyllum violaceum 3108257
103        Allophyllum violaceum            Allophyllum violaceum 4322220
104        Allophyllum violaceum            Allophyllum violaceum 4322220
105        Allophyllum violaceum            Allophyllum violaceum 3024210
106        Allophyllum violaceum            Allophyllum violaceum 3024210
107       Bonplandia geminiflora         Bonplandia geminiflora MO2606326
108       Bonplandia geminiflora        Bonplandia geminiflora MO04668787
109       Bonplandia geminiflora         Bonplandia geminiflora MO2606326
110       Bonplandia geminiflora        Bonplandia geminiflora MO04668787
111       Bonplandia geminiflora        Bonplandia geminiflora MO04668787
112       Bonplandia geminiflora         Bonplandia geminiflora MO2606326
113             Cantua buxifolia               Cantua buxifolia MO1284774
114             Cantua buxifolia               Cantua buxifolia MO6314998
115             Cantua buxifolia               Cantua buxifolia MO5782510
116             Cantua buxifolia               Cantua buxifolia MO6314998
117             Cantua buxifolia               Cantua buxifolia MO5782510
118             Cantua buxifolia               Cantua buxifolia MO5782510
119             Cantua buxifolia               Cantua buxifolia MO1284774
120             Cantua buxifolia               Cantua buxifolia MO6314998
121             Cantua buxifolia               Cantua buxifolia MO1284774
122            Cantua candelilla               Cantua candelilla MO587382
123            Cantua candelilla               Cantua candelilla MO587382
124            Cantua candelilla              Cantua candelilla MO6197258
125            Cantua candelilla              Cantua candelilla MO6197258
126            Cantua candelilla               Cantua candelilla MO587382
127            Cantua candelilla              Cantua candelilla MO6197258
128           Cantua quercifolia             Cantua quercifolia MO5782518
129           Cantua quercifolia             Cantua quercifolia MO5782509
130           Cantua quercifolia             Cantua quercifolia MO5782509
131           Cantua quercifolia             Cantua quercifolia MO5782509
132           Cantua quercifolia             Cantua quercifolia MO5782518
133           Cantua quercifolia             Cantua quercifolia MO5782518
134           Cantua quercifolia             Cantua quercifolia MO6030150
135           Cantua quercifolia             Cantua quercifolia MO6030150
136        Cobaea aequatoriensis        Cobaea aequatoriensis RSABG717593
137         Cobaea aschersoniana           Cobaea aschersoniana MO3602165
138           Cobaea campanulata           Cobaea campanulata RSABG697003
139                 Cobaea lutea                        Cobaea lutea NYBG
140                 Cobaea lutea                      Cobaea lutea NYBG 7
141                 Cobaea lutea                   Cobaea lutea MO5897922
142                 Cobaea lutea                   Cobaea lutea MO5897721
143                 Cobaea lutea                   Cobaea lutea MO6058036
144                 Cobaea lutea                      Cobaea lutea NYBG 5
145                 Cobaea lutea                   Cobaea lutea MO5897921
146                 Cobaea lutea                   Cobaea lutea MO5871652
147                 Cobaea lutea                      Cobaea lutea NYBG 6
148                 Cobaea lutea                      Cobaea lutea NYBG 2
149                 Cobaea lutea                      Cobaea lutea NYBG 3
150                 Cobaea minor                   Cobaea minor MO3861025
151                 Cobaea minor                   Cobaea minor MO3861033
152                 Cobaea minor                   Cobaea minor MO3861029
153          Cobaea penduliflora            Cobaea penduliflora MO2724237
154          Cobaea penduliflora             Cobaea penduliflora MO113696
155          Cobaea rotundiflora            Cobaea rotundiflora MO3750483
156          Cobaea rotundiflora            Cobaea rotundiflora MO3750483
157              Cobaea scandens                Cobaea scandens FLAS87478
158              Cobaea scandens                Cobaea scandens FLAS87478
159              Cobaea scandens                Cobaea scandens FLAS87478
160              Cobaea scandens               Cobaea scandens FLAS221362
161               Cobaea trianae                 Cobaea trianae MO6318229
162               Cobaea trianae                 Cobaea trianae MO3857537
163               Cobaea trianae                 Cobaea trianae MO6318229
164               Cobaea trianae                 Cobaea trianae MO3612189
165         Collomia grandiflora          Collomia grandiflora KSC0126124
166         Collomia grandiflora          Collomia grandiflora KANU363773
167         Collomia grandiflora           Collomia grandiflora FLAS53043
168         Collomia grandiflora           Collomia grandiflora FLAS53064
169         Collomia grandiflora           Collomia grandiflora FLAS53043
170         Collomia grandiflora          Collomia grandiflora KSC0126124
171         Collomia grandiflora          Collomia grandiflora FLAS124816
172         Collomia grandiflora           Collomia grandiflora FLAS53043
173         Collomia grandiflora          Collomia grandiflora KANU363773
174         Collomia grandiflora          Collomia grandiflora KANU363773
175         Collomia grandiflora           Collomia grandiflora FLAS55664
176         Collomia grandiflora           Collomia grandiflora FLAS55664
177         Collomia grandiflora           Collomia grandiflora FLAS53064
178         Collomia grandiflora          Collomia grandiflora KSC0126126
179         Collomia grandiflora          Collomia grandiflora KSC0126126
180         Collomia grandiflora           Collomia grandiflora FLAS53064
181         Collomia grandiflora        Collomia grandiflora KANU00218896
182         Collomia grandiflora        Collomia grandiflora KANU00218896
183         Collomia grandiflora           Collomia grandiflora FLAS55664
184         Collomia grandiflora          Collomia grandiflora KSC0126124
185         Collomia grandiflora          Collomia grandiflora KSC0126125
186         Collomia grandiflora          Collomia grandiflora KSC0126125
187         Collomia grandiflora          Collomia grandiflora KSC0126125
188         Collomia grandiflora          Collomia grandiflora KSC0126126
189         Collomia grandiflora        Collomia grandiflora KANU00218896
190        Collomia heterophylla           Collomia heterophylla FLAS5666
191        Collomia heterophylla       Collomia heterophylla KANU00218904
192        Collomia heterophylla         Collomia heterophylla KSC0126123
193        Collomia heterophylla       Collomia heterophylla KANU00218904
194        Collomia heterophylla       Collomia heterophylla KANU00218899
195        Collomia heterophylla          Collomia heterophylla FLAS53044
196        Collomia heterophylla       Collomia heterophylla KANU00218899
197        Collomia heterophylla       Collomia heterophylla KANU00218902
198        Collomia heterophylla           Collomia heterophylla FLAS5666
199        Collomia heterophylla       Collomia heterophylla KANU00218899
200        Collomia heterophylla       Collomia heterophylla KANU00218904
201        Collomia heterophylla           Collomia heterophylla FLAS5666
202        Collomia heterophylla          Collomia heterophylla FLAS72212
203        Collomia heterophylla          Collomia heterophylla FLAS55665
204        Collomia heterophylla          Collomia heterophylla FLAS53044
205        Collomia heterophylla          Collomia heterophylla FLAS72212
206        Collomia heterophylla         Collomia heterophylla KSC0126127
207        Collomia heterophylla       Collomia heterophylla KANU00218902
208        Collomia heterophylla          Collomia heterophylla FLAS72212
209        Collomia heterophylla          Collomia heterophylla FLAS55665
210        Collomia heterophylla         Collomia heterophylla KSC0126128
211        Collomia heterophylla          Collomia heterophylla FLAS55665
212        Collomia heterophylla       Collomia heterophylla KANU00218902
213        Collomia heterophylla         Collomia heterophylla FLAS118998
214        Collomia heterophylla         Collomia heterophylla KSC0126128
215        Collomia heterophylla         Collomia heterophylla KSC0126123
216        Collomia heterophylla          Collomia heterophylla FLAS53179
217        Collomia heterophylla         Collomia heterophylla KSC0126127
218        Collomia heterophylla          Collomia heterophylla FLAS53179
219        Collomia heterophylla         Collomia heterophylla KSC0126123
220        Collomia heterophylla          Collomia heterophylla FLAS53179
221        Collomia heterophylla         Collomia heterophylla KSC0126128
222        Collomia heterophylla         Collomia heterophylla FLAS118998
223        Collomia heterophylla          Collomia heterophylla FLAS53044
224        Collomia heterophylla          Collomia heterophylla FLAS53301
225        Collomia heterophylla         Collomia heterophylla KSC0126127
226        Collomia heterophylla          Collomia heterophylla FLAS53301
227        Collomia heterophylla         Collomia heterophylla FLAS118998
228            Collomia linearis           Collomia linearis KANU00326484
229            Collomia linearis           Collomia linearis KANU00326484
230            Collomia linearis              Collomia linearis FLAS57177
231            Collomia linearis             Collomia linearis FLAS190754
232            Collomia linearis             Collomia linearis KSC0126151
233            Collomia linearis              Collomia linearis FLAS20300
234            Collomia linearis           Collomia linearis KANU00326484
235            Collomia linearis             Collomia linearis FLAS190754
236            Collomia linearis             Collomia linearis KSC0126156
237            Collomia linearis              Collomia linearis FLAS44045
238            Collomia linearis              Collomia linearis FLAS40535
239            Collomia linearis              Collomia linearis FLAS40535
240            Collomia linearis             Collomia linearis KSC0126154
241            Collomia linearis           Collomia linearis KANU00218981
242            Collomia linearis              Collomia linearis FLAS57177
243            Collomia linearis             Collomia linearis FLAS147432
244            Collomia linearis              Collomia linearis FLAS40535
245            Collomia linearis           Collomia linearis KANU00218987
246            Collomia linearis             Collomia linearis KSC0126151
247            Collomia linearis              Collomia linearis FLAS20300
248            Collomia linearis           Collomia linearis KANU00218987
249            Collomia linearis             Collomia linearis FLAS147432
250            Collomia linearis           Collomia linearis KANU00218981
251            Collomia linearis             Collomia linearis FLAS190754
252            Collomia linearis             Collomia linearis FLAS122937
253            Collomia linearis              Collomia linearis FLAS20300
254            Collomia linearis           Collomia linearis KANU00218987
255            Collomia linearis             Collomia linearis FLAS122937
256            Collomia linearis              Collomia linearis FLAS44045
257            Collomia linearis             Collomia linearis KSC0126156
258            Collomia linearis             Collomia linearis KSC0126156
259            Collomia linearis           Collomia linearis KANU00218981
260            Collomia linearis              Collomia linearis FLAS57177
261            Collomia linearis             Collomia linearis KSC0126154
262            Collomia linearis             Collomia linearis KSC0126154
263            Collomia linearis              Collomia linearis FLAS44045
264            Collomia linearis             Collomia linearis FLAS147432
265            Collomia linearis             Collomia linearis FLAS122937
266            Collomia linearis             Collomia linearis KSC0126151
267              Collomia mazama                Collomia mazama MO5416813
268              Collomia mazama                Collomia mazama MO1285240
269              Collomia mazama                Collomia mazama MO1285240
270              Collomia mazama                Collomia mazama MO5416813
271              Collomia mazama                Collomia mazama MO5416813
272              Collomia mazama                Collomia mazama MO1285240
273          Collomia rawsoniana             Collomia rawsoniana MO933189
274          Collomia rawsoniana             Collomia rawsoniana MO933189
275          Collomia rawsoniana            Collomia rawsoniana MO2754561
276          Collomia rawsoniana             Collomia rawsoniana MO933189
277          Collomia rawsoniana             Collomia rawsoniana MO119646
278          Collomia rawsoniana             Collomia rawsoniana MO119646
279          Collomia rawsoniana            Collomia rawsoniana MO2754561
280          Collomia rawsoniana             Collomia rawsoniana MO119646
281          Collomia rawsoniana            Collomia rawsoniana MO2754561
282           Collomia tinctoria            Collomia tinctoria KANU360011
283           Collomia tinctoria            Collomia tinctoria KANU360011
284           Collomia tinctoria            Collomia tinctoria KANU360011
285           Collomia tinctoria            Collomia tinctoria KANU367450
286           Collomia tinctoria          Collomia tinctoria KANU00219029
287           Collomia tinctoria            Collomia tinctoria KANU367450
288           Collomia tinctoria          Collomia tinctoria KANU00219029
289           Collomia tinctoria          Collomia tinctoria KANU00219029
290           Collomia tinctoria            Collomia tinctoria KANU367450
291                Dayia grantii                Dayia grantii RSABG672123
292                Dayia grantii                Dayia grantii RSABG672123
293        Eriastrum densifolium         Eriastrum densifolium KSC0126183
294        Eriastrum densifolium         Eriastrum densifolium KSC0126235
295        Eriastrum densifolium       Eriastrum densifolium KANU00229151
296        Eriastrum densifolium       Eriastrum densifolium KANU00229151
297        Eriastrum densifolium         Eriastrum densifolium KSC0126236
298        Eriastrum densifolium          Eriastrum densifolium FLAS40522
299        Eriastrum densifolium       Eriastrum densifolium KANU00229153
300        Eriastrum densifolium       Eriastrum densifolium KANU00229150
301        Eriastrum densifolium         Eriastrum densifolium KSC0126235
302        Eriastrum densifolium       Eriastrum densifolium KANU00229150
303        Eriastrum densifolium       Eriastrum densifolium KANU00229151
304        Eriastrum densifolium         Eriastrum densifolium KSC0126236
305        Eriastrum densifolium         Eriastrum densifolium KSC0126235
306        Eriastrum densifolium       Eriastrum densifolium KANU00229153
307        Eriastrum densifolium         Eriastrum densifolium KSC0126183
308        Eriastrum densifolium       Eriastrum densifolium KANU00229153
309        Eriastrum densifolium         Eriastrum densifolium KSC0126183
310        Eriastrum densifolium          Eriastrum densifolium FLAS40522
311        Eriastrum densifolium         Eriastrum densifolium KSC0126236
312        Eriastrum densifolium       Eriastrum densifolium KANU00229150
313        Eriastrum densifolium          Eriastrum densifolium FLAS40522
314           Eriastrum eremicum            Eriastrum eremicum KSC0126181
315           Eriastrum eremicum            Eriastrum eremicum KANI361667
316           Eriastrum eremicum            Eriastrum eremicum FLAS158631
317           Eriastrum eremicum            Eriastrum eremicum KSC0126182
318           Eriastrum eremicum            Eriastrum eremicum KANI361667
319           Eriastrum eremicum            Eriastrum eremicum KANI361667
320           Eriastrum eremicum            Eriastrum eremicum KSC0126182
321           Eriastrum eremicum            Eriastrum eremicum KSC0126182
322           Eriastrum eremicum            Eriastrum eremicum FLAS158631
323           Eriastrum eremicum            Eriastrum eremicum KSC0126274
324           Eriastrum eremicum          Eriastrum eremicum KANU00229159
325           Eriastrum eremicum          Eriastrum eremicum KANU00229158
326           Eriastrum eremicum            Eriastrum eremicum FLAS158631
327           Eriastrum eremicum            Eriastrum eremicum KSC0126274
328           Eriastrum eremicum            Eriastrum eremicum KSC0126274
329           Eriastrum eremicum          Eriastrum eremicum KANU00229159
330           Eriastrum eremicum          Eriastrum eremicum KANU00229158
331           Eriastrum eremicum          Eriastrum eremicum KANU00229158
332           Eriastrum eremicum            Eriastrum eremicum KSC0126181
333           Eriastrum eremicum          Eriastrum eremicum KANU00229159
334           Eriastrum eremicum            Eriastrum eremicum KSC0126181
335             Eriastrum luteum               Eriastrum luteum MO2753702
336             Eriastrum luteum               Eriastrum luteum MO2753700
337             Eriastrum luteum             Eriastrum luteum RSABG523184
338             Eriastrum luteum             Eriastrum luteum RSABG523184
339             Eriastrum luteum               Eriastrum luteum MO2753700
340             Eriastrum luteum             Eriastrum luteum RSABG523184
341             Eriastrum luteum               Eriastrum luteum MO2753700
342        Eriastrum sapphirinum       Eriastrum sapphirinum KANU00229171
343        Eriastrum sapphirinum         Eriastrum sapphirinum FLAS124728
344        Eriastrum sapphirinum         Eriastrum sapphirinum FLAS124728
345        Eriastrum sapphirinum         Eriastrum sapphirinum FLAS124728
346        Eriastrum sapphirinum       Eriastrum sapphirinum KANU00229171
347        Eriastrum sapphirinum       Eriastrum sapphirinum KANU00229171
348           Eriastrum wilcoxii          Eriastrum wilcoxii KANU00229181
349           Eriastrum wilcoxii            Eriastrum wilcoxii KSC0126373
350           Eriastrum wilcoxii             Eriastrum wilcoxii FLAS20307
351           Eriastrum wilcoxii             Eriastrum wilcoxii FLAS20307
352           Eriastrum wilcoxii             Eriastrum wilcoxii FLAS20307
353           Eriastrum wilcoxii            Eriastrum wilcoxii KSC0126373
354           Eriastrum wilcoxii            Eriastrum wilcoxii KSC0126373
355         Fouquieria splendens           Fouquieria splendens MO2294377
356         Fouquieria splendens           Fouquieria splendens MO1963133
357         Fouquieria splendens           Fouquieria splendens MO1963130
358         Fouquieria splendens           Fouquieria splendens MO1963130
359         Fouquieria splendens           Fouquieria splendens MO1963133
360         Fouquieria splendens           Fouquieria splendens MO1963133
361         Fouquieria splendens           Fouquieria splendens MO2294377
362         Fouquieria splendens           Fouquieria splendens MO2294377
363         Fouquieria splendens           Fouquieria splendens MO1963130
364              Gilia aliquanta               Gilia aliquanta MO05091925
365              Gilia aliquanta                Gilia aliquanta MO2985153
366              Gilia aliquanta               Gilia aliquanta MO05091925
367              Gilia aliquanta               Gilia aliquanta MO05091926
368              Gilia aliquanta               Gilia aliquanta MO05091925
369              Gilia aliquanta                Gilia aliquanta MO2985153
370              Gilia aliquanta                Gilia aliquanta MO2985153
371              Gilia aliquanta               Gilia aliquanta MO05091926
372              Gilia aliquanta               Gilia aliquanta MO05091926
373             Gilia angelensis              Gilia angelensis KANU352958
374             Gilia angelensis              Gilia angelensis FLAS124815
375             Gilia angelensis              Gilia angelensis KANU352958
376             Gilia angelensis              Gilia angelensis FLAS103491
377             Gilia angelensis              Gilia angelensis FLAS103491
378             Gilia angelensis            Gilia angelensis KANU00237600
379             Gilia angelensis              Gilia angelensis FLAS103491
380             Gilia angelensis            Gilia angelensis KANU00237600
381             Gilia angelensis            Gilia angelensis KANU00237600
382             Gilia angelensis              Gilia angelensis FLAS124815
383             Gilia angelensis              Gilia angelensis FLAS124815
384             Gilia angelensis              Gilia angelensis KANU352958
385     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761527
386     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761527
387     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761740
388     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761740
389     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761527
390     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761740
391                   Gilia cana                    Gilia cana KSC0126225
392                   Gilia cana                    Gilia cana KSC0126225
393                   Gilia cana                  Gilia cana KANU00237606
394                   Gilia cana                    Gilia cana KSC0126225
395                   Gilia cana                  Gilia cana KANU00237606
396                   Gilia cana                  Gilia cana KANU00237606
397               Gilia capitata                Gilia capitata KSC0126218
398               Gilia capitata        Gilia capitata KANU365398 flowers
399               Gilia capitata                Gilia capitata KSC0126218
400               Gilia capitata                Gilia capitata KSC0126215
401               Gilia capitata                Gilia capitata KSC0126215
402               Gilia capitata        Gilia capitata KANU365398 flowers
403               Gilia capitata                 Gilia capitata FLAS20303
404               Gilia capitata                 Gilia capitata FLAS55667
405               Gilia capitata                Gilia capitata KANU365398
406               Gilia capitata                 Gilia capitata FLAS55667
407               Gilia capitata                Gilia capitata KANU365398
408               Gilia capitata                 Gilia capitata FLAS55668
409               Gilia capitata                Gilia capitata KSC0126218
410               Gilia capitata                Gilia capitata KANU365398
411               Gilia capitata                Gilia capitata KSC0126215
412               Gilia capitata              Gilia capitata KANU00237624
413               Gilia capitata                 Gilia capitata FLAS20303
414               Gilia capitata              Gilia capitata KANU00237624
415               Gilia capitata                 Gilia capitata FLAS55667
416               Gilia capitata                 Gilia capitata FLAS20303
417               Gilia capitata        Gilia capitata KANU365398 flowers
418               Gilia capitata              Gilia capitata KANU00237624
419               Gilia capitata                 Gilia capitata FLAS55668
420               Gilia capitata                 Gilia capitata FLAS55668
421               Gilia clivorum                Gilia clivorum KANU365951
422               Gilia clivorum              Gilia clivorum KANU00237635
423               Gilia clivorum                Gilia clivorum KANU365951
424               Gilia clivorum              Gilia clivorum KANU00237635
425               Gilia clivorum              Gilia clivorum KANU00237635
426               Gilia clivorum                Gilia clivorum KANU365951
427                Gilia clokeyi               Gilia clokeyi KANU00237636
428                Gilia clokeyi               Gilia clokeyi KANU00237636
429                Gilia clokeyi               Gilia clokeyi KANU00237637
430                Gilia clokeyi               Gilia clokeyi KANU00237636
431                Gilia clokeyi               Gilia clokeyi KANU00237637
432                Gilia clokeyi               Gilia clokeyi KANU00237637
433            Gilia crassifolia               Gilia crassifolia MO910596
434            Gilia crassifolia               Gilia crassifolia MO910596
435            Gilia crassifolia              Gilia crassifolia MO6217685
436            Gilia crassifolia               Gilia crassifolia MO910596
437            Gilia crassifolia              Gilia crassifolia MO6217685
438            Gilia crassifolia              Gilia crassifolia MO6217685
439            Gilia crassifolia              Gilia crassifolia MO2415702
440            Gilia crassifolia              Gilia crassifolia MO2415702
441            Gilia crassifolia              Gilia crassifolia MO2415702
442              Gilia diegensis                 Gilia diegensis UF124844
443              Gilia diegensis               Gilia diegensis FLAS124844
444              Gilia diegensis               Gilia diegensis FLAS124844
445              Gilia diegensis               Gilia diegensis FLAS124844
446              Gilia diegensis                 Gilia diegensis UF124844
447              Gilia diegensis                 Gilia diegensis UF124844
448            Gilia inconspicua             Gilia inconspicua KSC0126288
449            Gilia inconspicua                Gilia inconspicua UF71466
450            Gilia inconspicua             Gilia inconspicua KSC0126288
451            Gilia inconspicua             Gilia inconspicua KSC0126288
452            Gilia inconspicua           Gilia inconspicua KANU00237648
453            Gilia inconspicua              Gilia inconspicua FLAS71466
454            Gilia inconspicua                Gilia inconspicua UF71466
455            Gilia inconspicua              Gilia inconspicua FLAS71466
456            Gilia inconspicua             Gilia inconspicua KSC0126285
457            Gilia inconspicua                Gilia inconspicua UF71466
458            Gilia inconspicua           Gilia inconspicua KANU00237648
459            Gilia inconspicua             Gilia inconspicua KSC0126285
460            Gilia inconspicua           Gilia inconspicua KANU00237648
461            Gilia inconspicua              Gilia inconspicua FLAS71466
462            Gilia inconspicua             Gilia inconspicua KANU354829
463            Gilia inconspicua             Gilia inconspicua KANU354833
464            Gilia inconspicua             Gilia inconspicua KANU354829
465            Gilia inconspicua             Gilia inconspicua KANU354829
466            Gilia inconspicua             Gilia inconspicua KSC0126285
467            Gilia inconspicua             Gilia inconspicua KANU354833
468            Gilia inconspicua             Gilia inconspicua KSC0126280
469            Gilia inconspicua             Gilia inconspicua KSC0126280
470            Gilia inconspicua             Gilia inconspicua KANU354833
471            Gilia inconspicua             Gilia inconspicua KSC0126280
472               Gilia interior                 Gilia interior MO2684705
473               Gilia interior                 Gilia interior MO2684705
474               Gilia interior                 Gilia interior MO2684705
475              Gilia laciniata              Gilia laciniata RSABG670177
476              Gilia laciniata              Gilia laciniata RSABG742105
477              Gilia laciniata              Gilia laciniata RSABG670177
478              Gilia laciniata              Gilia laciniata RSABG742105
479              Gilia laciniata              Gilia laciniata RSABG670177
480              Gilia laciniata              Gilia laciniata RSABG742105
481              Gilia latiflora             Gilia latiflora KANU00237664
482              Gilia latiflora             Gilia latiflora KANU00237664
483              Gilia latiflora             Gilia latiflora KANU00237670
484              Gilia latiflora             Gilia latiflora KANU00237664
485              Gilia latiflora             Gilia latiflora KANU00237670
486              Gilia latiflora             Gilia latiflora KANU00237665
487              Gilia latiflora             Gilia latiflora KANU00237670
488              Gilia latiflora             Gilia latiflora KANU00237665
489              Gilia latiflora             Gilia latiflora KANU00237665
490              Gilia leptantha                Gilia leptantha MO1971214
491              Gilia leptantha                Gilia leptantha MO1971214
492              Gilia leptantha                Gilia leptantha MO1971214
493                 Gilia malior                Gilia malior KANU00329179
494                 Gilia malior                Gilia malior KANU00329179
495               Gilia mexicana                 Gilia mexicana MO2926797
496               Gilia mexicana                 Gilia mexicana MO2926797
497               Gilia mexicana                 Gilia mexicana MO2926797
498           Gilia millefoliata          Gilia millefoliata KANU00237704
499           Gilia millefoliata          Gilia millefoliata KANU00237704
500           Gilia millefoliata          Gilia millefoliata KANU00237704
501                  Gilia minor                 Gilia minor KANU00237705
502                  Gilia minor                 Gilia minor KANU00237705
503                  Gilia minor                 Gilia minor KANU00237705
504                Gilia nevinii                  Gilia nevinii MO2754968
505                Gilia nevinii                  Gilia nevinii MO2397657
506                Gilia nevinii                  Gilia nevinii MO2754968
507                Gilia nevinii                  Gilia nevinii MO2397657
508                Gilia nevinii                  Gilia nevinii MO2397657
509                Gilia nevinii                  Gilia nevinii MO2753771
510                Gilia nevinii                  Gilia nevinii MO2754968
511                Gilia sinuata                 Gilia sinuata KSC0126407
512                Gilia sinuata               Gilia sinuata KANU00237779
513                Gilia sinuata                 Gilia sinuata KANU365726
514                Gilia sinuata               Gilia sinuata KANU00237781
515                Gilia sinuata                    Gilia sinuata UF79291
516                Gilia sinuata               Gilia sinuata KANU00237779
517                Gilia sinuata               Gilia sinuata KANU00237781
518                Gilia sinuata                    Gilia sinuata UF79291
519                Gilia sinuata               Gilia sinuata KANU00237779
520                Gilia sinuata                 Gilia sinuata KSC0126407
521                Gilia sinuata               Gilia sinuata KANU00237781
522                Gilia sinuata                 Gilia sinuata KANU365726
523                Gilia sinuata                 Gilia sinuata KSC0126407
524                Gilia sinuata                 Gilia sinuata KANU365726
525                Gilia sinuata                    Gilia sinuata UF79291
526               Gilia stellata              Gilia stellata KANU00237789
527               Gilia stellata              Gilia stellata KANU00237788
528               Gilia stellata              Gilia stellata KANU00237788
529               Gilia stellata                  Gilia stellata UF106799
530               Gilia stellata              Gilia stellata KANU00237787
531               Gilia stellata              Gilia stellata KANU00237788
532               Gilia stellata              Gilia stellata KANU00237789
533               Gilia stellata              Gilia stellata KANU00237787
534               Gilia stellata                  Gilia stellata UF106799
535               Gilia stellata              Gilia stellata KANU00237787
536               Gilia stellata                  Gilia stellata UF106799
537               Gilia stellata              Gilia stellata KANU00237789
538             Gilia tenuiflora    Gilia tenuiflora KANU00237663 flowers
539             Gilia tenuiflora    Gilia tenuiflora KANU00237663 flowers
540             Gilia tenuiflora            Gilia tenuiflora KANU00237808
541             Gilia tenuiflora            Gilia tenuiflora KANU00237808
542             Gilia tenuiflora            Gilia tenuiflora KANU00237663
543             Gilia tenuiflora    Gilia tenuiflora KANU00237663 flowers
544             Gilia tenuiflora              Gilia tenuiflora KSC0126388
545             Gilia tenuiflora            Gilia tenuiflora KANU00237809
546             Gilia tenuiflora            Gilia tenuiflora KANU00237809
547             Gilia tenuiflora              Gilia tenuiflora KSC0126388
548             Gilia tenuiflora            Gilia tenuiflora KANU00237810
549             Gilia tenuiflora            Gilia tenuiflora KANU00237808
550             Gilia tenuiflora              Gilia tenuiflora KSC0126388
551             Gilia tenuiflora            Gilia tenuiflora KANU00237810
552             Gilia tenuiflora            Gilia tenuiflora KANU00237809
553             Gilia tenuiflora            Gilia tenuiflora KANU00237663
554             Gilia tenuiflora            Gilia tenuiflora KANU00237663
555             Gilia tenuiflora            Gilia tenuiflora KANU00237810
556           Gilia transmontana          Gilia transmontana KANU00237813
557           Gilia transmontana          Gilia transmontana KANU00237813
558           Gilia transmontana          Gilia transmontana KANU00237813
559               Gilia tricolor                Gilia tricolor KANU365399
560               Gilia tricolor              Gilia tricolor KANU00237815
561               Gilia tricolor              Gilia tricolor KANU00329192
562               Gilia tricolor              Gilia tricolor KANU00329192
563               Gilia tricolor              Gilia tricolor KANU00329192
564               Gilia tricolor                Gilia tricolor KSC0126382
565               Gilia tricolor                  Gilia tricolor UF122313
566               Gilia tricolor              Gilia tricolor KANU00237815
567               Gilia tricolor              Gilia tricolor KANU00237815
568               Gilia tricolor                  Gilia tricolor UF122313
569               Gilia tricolor                Gilia tricolor KSC0126382
570               Gilia tricolor                Gilia tricolor KANU365399
571               Gilia tricolor                Gilia tricolor KANU365399
572               Gilia tricolor                  Gilia tricolor UF122313
573               Gilia tricolor                Gilia tricolor KSC0126382
574                Gilia tweedyi               Gilia tweedyi KANU00237816
575                Gilia tweedyi               Gilia tweedyi KANU00307908
576                Gilia tweedyi               Gilia tweedyi KANU00307908
577                Gilia tweedyi               Gilia tweedyi KANU00237816
578                Gilia tweedyi                 Gilia tweedyi KSC0126379
579                Gilia tweedyi               Gilia tweedyi KANU00237816
580                Gilia tweedyi                 Gilia tweedyi KSC0126379
581                Gilia tweedyi                 Gilia tweedyi KSC0126379
582                Gilia tweedyi               Gilia tweedyi KANU00307908
583           Gilia valdiviensis           Gilia valdiviensis RSABG689440
584           Gilia valdiviensis           Gilia valdiviensis RSABG689440
585           Gilia valdiviensis           Gilia valdiviensis RSABG742098
586           Gilia valdiviensis           Gilia valdiviensis RSABG742098
587           Gilia valdiviensis           Gilia valdiviensis RSABG689440
588          Giliastrum foetidum            Giliastrum foetidum MO2387040
589          Giliastrum foetidum            Giliastrum foetidum MO3381110
590          Giliastrum foetidum            Giliastrum foetidum MO5691534
591          Giliastrum foetidum            Giliastrum foetidum MO5691534
592          Giliastrum foetidum            Giliastrum foetidum MO2387040
593          Giliastrum foetidum            Giliastrum foetidum MO3381110
594          Giliastrum foetidum            Giliastrum foetidum MO2387040
595          Giliastrum foetidum            Giliastrum foetidum MO5691534
596           Giliastrum incisum          Giliastrum incisum KANU00237644
597           Giliastrum incisum          Giliastrum incisum KANU00237644
598           Giliastrum incisum               Giliastrum incisum UF97704
599           Giliastrum incisum          Giliastrum incisum KANU00237644
600           Giliastrum incisum            Giliastrum incisum KANU349179
601           Giliastrum incisum              Giliastrum incisum UF151811
602           Giliastrum incisum               Giliastrum incisum UF97704
603           Giliastrum incisum            Giliastrum incisum KANU349179
604           Giliastrum incisum            Giliastrum incisum KANU349179
605           Giliastrum incisum              Giliastrum incisum UF151811
606           Giliastrum incisum               Giliastrum incisum UF97704
607           Giliastrum incisum            Giliastrum incisum FLAS151811
608           Giliastrum incisum              Giliastrum incisum UF151811
609           Giliastrum incisum            Giliastrum incisum FLAS151811
610           Giliastrum incisum            Giliastrum incisum FLAS151811
611       Gymnosteris nudicaulis      Gymnosteris nudicaulis KANU00238204
612       Gymnosteris nudicaulis      Gymnosteris nudicaulis KANU00238204
613       Gymnosteris nudicaulis      Gymnosteris nudicaulis KANU00238204
614          Gymnosteris parvula         Gymnosteris parvula KANU00238206
615          Gymnosteris parvula         Gymnosteris parvula KANU00238205
616          Gymnosteris parvula         Gymnosteris parvula KANU00238206
617          Gymnosteris parvula         Gymnosteris parvula KANU00238206
618          Gymnosteris parvula         Gymnosteris parvula KANU00238205
619          Gymnosteris parvula         Gymnosteris parvula KANU00238205
620          Ipomopsis arizonica         Ipomopsis arizonica KANU00242038
621          Ipomopsis arizonica         Ipomopsis arizonica KANU00242026
622          Ipomopsis arizonica         Ipomopsis arizonica KANU00242034
623          Ipomopsis arizonica         Ipomopsis arizonica KANU00242034
624          Ipomopsis arizonica         Ipomopsis arizonica KANU00242026
625          Ipomopsis arizonica           Ipomopsis arizonica KSC0126427
626          Ipomopsis arizonica            Ipomopsis arizonica RMH369047
627          Ipomopsis arizonica           Ipomopsis arizonica KSC0126427
628          Ipomopsis arizonica         Ipomopsis arizonica KANU00242034
629          Ipomopsis arizonica           Ipomopsis arizonica KSC0126425
630          Ipomopsis arizonica            Ipomopsis arizonica RMH152770
631          Ipomopsis arizonica            Ipomopsis arizonica RMH369047
632          Ipomopsis arizonica         Ipomopsis arizonica KANU00242038
633          Ipomopsis arizonica           Ipomopsis arizonica KSC0126425
634          Ipomopsis arizonica            Ipomopsis arizonica RMH369047
635          Ipomopsis arizonica            Ipomopsis arizonica RMH152770
636          Ipomopsis arizonica           Ipomopsis arizonica KSC0126427
637          Ipomopsis arizonica           Ipomopsis arizonica KSC0126425
638          Ipomopsis arizonica            Ipomopsis arizonica RMH152770
639          Ipomopsis arizonica         Ipomopsis arizonica KANU00242038
640          Ipomopsis arizonica         Ipomopsis arizonica KANU00242026
641           Ipomopsis congesta            Ipomopsis congesta FLAS202042
642           Ipomopsis congesta             Ipomopsis congesta FLAS43707
643           Ipomopsis congesta            Ipomopsis congesta FLAS202042
644           Ipomopsis congesta               Ipomopsis congesta UF43707
645           Ipomopsis congesta            Ipomopsis congesta FLAS202042
646           Ipomopsis congesta             Ipomopsis congesta FLAS43707
647           Ipomopsis congesta             Ipomopsis congesta FLAS43707
648           Ipomopsis congesta               Ipomopsis congesta UF43707
649           Ipomopsis congesta               Ipomopsis congesta UF43707
650        Ipomopsis gossypifera          Ipomopsis gossypifera MO2990603
651        Ipomopsis gossypifera         Ipomopsis gossypifera MO04674924
652        Ipomopsis gossypifera          Ipomopsis gossypifera MO2990603
653        Ipomopsis gossypifera          Ipomopsis gossypifera MO2990603
654        Ipomopsis gossypifera         Ipomopsis gossypifera MO04674924
655        Ipomopsis gossypifera         Ipomopsis gossypifera MO04674924
656        Ipomopsis gossypifera          Ipomopsis gossypifera MO5695559
657        Ipomopsis gossypifera          Ipomopsis gossypifera MO5695559
658        Ipomopsis gossypifera          Ipomopsis gossypifera MO5695559
659            Ipomopsis guttata           Ipomopsis guttata KANU00242128
660            Ipomopsis guttata              Ipomopsis guttata ASU136142
661            Ipomopsis guttata           Ipomopsis guttata KANU00242128
662            Ipomopsis guttata              Ipomopsis guttata ASU203122
663            Ipomopsis guttata              Ipomopsis guttata ASU136142
664            Ipomopsis guttata              Ipomopsis guttata ASU203122
665            Ipomopsis guttata           Ipomopsis guttata KANU00242128
666            Ipomopsis guttata              Ipomopsis guttata ASU203122
667            Ipomopsis guttata              Ipomopsis guttata ASU136142
668         Ipomopsis longiflora          Ipomopsis longiflora KANU365648
669         Ipomopsis longiflora          Ipomopsis longiflora KANU365648
670         Ipomopsis longiflora          Ipomopsis longiflora KANU365648
671         Ipomopsis longiflora          Ipomopsis longiflora KANU365567
672         Ipomopsis longiflora          Ipomopsis longiflora KANU365567
673         Ipomopsis longiflora          Ipomopsis longiflora KANU365567
674         Ipomopsis longiflora            Ipomopsis longiflora UNM96003
675         Ipomopsis longiflora         Ipomopsis longiflora KSC01226429
676         Ipomopsis longiflora        Ipomopsis longiflora KANU00314448
677         Ipomopsis longiflora          Ipomopsis longiflora KSC0126416
678         Ipomopsis longiflora          Ipomopsis longiflora KSC0126416
679         Ipomopsis longiflora        Ipomopsis longiflora KANU00314448
680         Ipomopsis longiflora         Ipomopsis longiflora KSC01226429
681         Ipomopsis longiflora            Ipomopsis longiflora UNM96003
682         Ipomopsis longiflora            Ipomopsis longiflora UNM96003
683         Ipomopsis longiflora          Ipomopsis longiflora KANU367093
684         Ipomopsis longiflora        Ipomopsis longiflora KANU00314448
685         Ipomopsis longiflora          Ipomopsis longiflora KANU367093
686         Ipomopsis longiflora          Ipomopsis longiflora KSC0126416
687         Ipomopsis longiflora          Ipomopsis longiflora KANU367093
688         Ipomopsis longiflora         Ipomopsis longiflora KSC01226429
689           Ipomopsis macombii             Ipomopsis macombii MO3537828
690           Ipomopsis macombii               Ipomopsis macombii UF54612
691           Ipomopsis macombii               Ipomopsis macombii UF54612
692           Ipomopsis macombii             Ipomopsis macombii MO3537828
693           Ipomopsis macombii             Ipomopsis macombii MO3537828
694           Ipomopsis macombii               Ipomopsis macombii UF20309
695           Ipomopsis macombii            Ipomopsis macombii MO04942860
696           Ipomopsis macombii               Ipomopsis macombii UF20309
697           Ipomopsis macombii               Ipomopsis macombii UF54612
698           Ipomopsis macombii             Ipomopsis macombii MO3667297
699           Ipomopsis macombii               Ipomopsis macombii UF57281
700           Ipomopsis macombii             Ipomopsis macombii MO3667297
701           Ipomopsis macombii               Ipomopsis macombii UF20309
702           Ipomopsis macombii             Ipomopsis macombii MO3667297
703           Ipomopsis macombii            Ipomopsis macombii MO04942860
704           Ipomopsis macombii            Ipomopsis macombii MO04942860
705           Ipomopsis macombii               Ipomopsis macombii UF57281
706           Ipomopsis macombii               Ipomopsis macombii UF57281
707            Ipomopsis pinnata            Ipomopsis pinnata RSABG743768
708            Ipomopsis pinnata            Ipomopsis pinnata RSABG586885
709            Ipomopsis pinnata            Ipomopsis pinnata RSABG743768
710            Ipomopsis pinnata               Ipomopsis pinnata UNM81872
711            Ipomopsis pinnata            Ipomopsis pinnata RSABG743768
712            Ipomopsis pinnata               Ipomopsis pinnata ASU79436
713            Ipomopsis pinnata              Ipomopsis pinnata UNM123448
714            Ipomopsis pinnata               Ipomopsis pinnata ASU79436
715            Ipomopsis pinnata            Ipomopsis pinnata RSABG586885
716            Ipomopsis pinnata               Ipomopsis pinnata UNM81872
717            Ipomopsis pinnata               Ipomopsis pinnata ASU79436
718            Ipomopsis pinnata              Ipomopsis pinnata UNM123448
719            Ipomopsis pinnata            Ipomopsis pinnata RSABG586885
720            Ipomopsis pinnata               Ipomopsis pinnata UNM81872
721            Ipomopsis pinnata              Ipomopsis pinnata UNM123448
722          Ipomopsis polyantha         Ipomopsis polyantha KANU00242201
723          Ipomopsis polyantha            Ipomopsis polyantha MO5188647
724          Ipomopsis polyantha              Ipomopsis polyantha 1199313
725          Ipomopsis polyantha              Ipomopsis polyantha 1199313
726          Ipomopsis polyantha           Ipomopsis polyantha FLAS134452
727          Ipomopsis polyantha            Ipomopsis polyantha RMH393646
728          Ipomopsis polyantha            Ipomopsis polyantha MO5188647
729          Ipomopsis polyantha              Ipomopsis polyantha 1199313
730          Ipomopsis polyantha            Ipomopsis polyantha MO5188647
731          Ipomopsis polyantha         Ipomopsis polyantha KANU00242201
732          Ipomopsis polyantha         Ipomopsis polyantha KANU00242201
733          Ipomopsis polyantha           Ipomopsis polyantha FLAS134452
734          Ipomopsis polyantha            Ipomopsis polyantha RMH393647
735          Ipomopsis polyantha             Ipomopsis polyantha MO890264
736          Ipomopsis polyantha           Ipomopsis polyantha FLAS134452
737          Ipomopsis polyantha            Ipomopsis polyantha RMH393647
738          Ipomopsis polyantha             Ipomopsis polyantha MO890264
739          Ipomopsis polyantha              Ipomopsis polyantha UF40538
740          Ipomopsis polyantha            Ipomopsis polyantha RMH393646
741          Ipomopsis polyantha              Ipomopsis polyantha UF40538
742          Ipomopsis polyantha            Ipomopsis polyantha RMH393647
743          Ipomopsis polyantha            Ipomopsis polyantha RMH393646
744          Ipomopsis polyantha              Ipomopsis polyantha UF40538
745          Ipomopsis polyantha             Ipomopsis polyantha MO890264
746         Ipomopsis polycladon        Ipomopsis polycladon KANU00242202
747         Ipomopsis polycladon        Ipomopsis polycladon KANU00242209
748         Ipomopsis polycladon        Ipomopsis polycladon KANU00242209
749         Ipomopsis polycladon        Ipomopsis polycladon KANU00242207
750         Ipomopsis polycladon        Ipomopsis polycladon KANU00242202
751         Ipomopsis polycladon        Ipomopsis polycladon KANU00242209
752         Ipomopsis polycladon        Ipomopsis polycladon KANU00242207
753         Ipomopsis polycladon        Ipomopsis polycladon KANU00242202
754             Ipomopsis pumila                Ipomopsis pumila UNM88300
755             Ipomopsis pumila              Ipomopsis pumila KSC0127470
756             Ipomopsis pumila                Ipomopsis pumila UNM93840
757             Ipomopsis pumila                Ipomopsis pumila UNM93840
758             Ipomopsis pumila            Ipomopsis pumila KANU00242210
759             Ipomopsis pumila            Ipomopsis pumila KANU00242213
760             Ipomopsis pumila               Ipomopsis pumila UNM113120
761             Ipomopsis pumila               Ipomopsis pumila UNM114147
762             Ipomopsis pumila            Ipomopsis pumila KANU00242210
763             Ipomopsis pumila               Ipomopsis pumila UNM113120
764             Ipomopsis pumila                Ipomopsis pumila UNM88300
765             Ipomopsis pumila            Ipomopsis pumila KANU00242210
766             Ipomopsis pumila                Ipomopsis pumila UNM93840
767             Ipomopsis pumila            Ipomopsis pumila KANU00242213
768             Ipomopsis pumila               Ipomopsis pumila UNM114147
769             Ipomopsis pumila            Ipomopsis pumila KANU00242213
770             Ipomopsis pumila               Ipomopsis pumila UNM113120
771             Ipomopsis pumila               Ipomopsis pumila UNM114147
772             Ipomopsis pumila              Ipomopsis pumila KSC0127470
773             Ipomopsis pumila                Ipomopsis pumila UNM88300
774             Ipomopsis pumila              Ipomopsis pumila KSC0127470
775            Ipomopsis roseata              Ipomopsis roseata RMH344766
776            Ipomopsis roseata              Ipomopsis roseata RMH744381
777            Ipomopsis roseata              Ipomopsis roseata RMH344766
778            Ipomopsis roseata           Ipomopsis roseata KANU00242219
779            Ipomopsis roseata           Ipomopsis roseata KANU00242218
780            Ipomopsis roseata           Ipomopsis roseata KANU00242219
781            Ipomopsis roseata              Ipomopsis roseata RMH744381
782            Ipomopsis roseata              Ipomopsis roseata RMH344766
783            Ipomopsis roseata              Ipomopsis roseata RMH785469
784            Ipomopsis roseata           Ipomopsis roseata KANU00242218
785            Ipomopsis roseata              Ipomopsis roseata RMH368362
786            Ipomopsis roseata              Ipomopsis roseata RMH785469
787            Ipomopsis roseata              Ipomopsis roseata RMH744381
788            Ipomopsis roseata              Ipomopsis roseata RMH785469
789            Ipomopsis roseata              Ipomopsis roseata RMH368362
790            Ipomopsis roseata              Ipomopsis roseata RMH368362
791            Ipomopsis roseata           Ipomopsis roseata KANU00242219
792            Ipomopsis roseata           Ipomopsis roseata KANU00242218
793              Ipomopsis rubra               Ipomopsis rubra KANU352624
794              Ipomopsis rubra               Ipomopsis rubra KANU352624
795              Ipomopsis rubra             Ipomopsis rubra KANU00242224
796              Ipomopsis rubra               Ipomopsis rubra KANU352624
797              Ipomopsis rubra             Ipomopsis rubra KANU00242224
798              Ipomopsis rubra             Ipomopsis rubra KANU00242224
799         Ipomopsis tenuifolia        Ipomopsis tenuifolia KANU00237806
800         Ipomopsis tenuifolia        Ipomopsis tenuifolia KANU00237806
801          Ipomopsis tenuituba            Ipomopsis tenuituba ASU281978
802          Ipomopsis tenuituba            Ipomopsis tenuituba ASU208786
803          Ipomopsis tenuituba            Ipomopsis tenuituba ASU281978
804          Ipomopsis tenuituba            Ipomopsis tenuituba ASU208786
805          Ipomopsis tenuituba            Ipomopsis tenuituba ASU208786
806          Ipomopsis tenuituba            Ipomopsis tenuituba ASU281978
807           Ipomopsis thurberi             Ipomopsis thurberi ASU193998
808           Ipomopsis thurberi             Ipomopsis thurberi ASU193998
809           Ipomopsis thurberi          Ipomopsis thurberi KANU00242276
810           Ipomopsis thurberi             Ipomopsis thurberi ASU262383
811           Ipomopsis thurberi            Ipomopsis thurberi KANU355538
812           Ipomopsis thurberi             Ipomopsis thurberi ASU262383
813           Ipomopsis thurberi            Ipomopsis thurberi KANU355538
814           Ipomopsis thurberi             Ipomopsis thurberi ASU193998
815           Ipomopsis thurberi            Ipomopsis thurberi KSC0126411
816           Ipomopsis thurberi            Ipomopsis thurberi KANU355538
817           Ipomopsis thurberi          Ipomopsis thurberi KANU00242276
818           Ipomopsis thurberi             Ipomopsis thurberi ASU262383
819           Ipomopsis thurberi            Ipomopsis thurberi KSC0126411
820           Ipomopsis thurberi            Ipomopsis thurberi KSC0126411
821           Ipomopsis thurberi          Ipomopsis thurberi KANU00242276
822          Langloisia punctata         Langloisia punctata KANU00245452
823          Langloisia punctata         Langloisia punctata KANU00245454
824          Langloisia punctata           Langloisia punctata KANU358618
825          Langloisia punctata           Langloisia punctata KANU358618
826          Langloisia punctata         Langloisia punctata KANU00245454
827          Langloisia punctata           Langloisia punctata KANU358618
828          Langloisia punctata         Langloisia punctata KANU00245452
829          Langloisia punctata         Langloisia punctata KANU00245454
830          Langloisia punctata         Langloisia punctata KANU00245452
831       Langloisia setosissima      Langloisia setosissima KANU00245459
832       Langloisia setosissima        Langloisia setosissima KSC0126435
833       Langloisia setosissima        Langloisia setosissima KSC0126435
834       Langloisia setosissima      Langloisia setosissima KANU00245459
835       Langloisia setosissima      Langloisia setosissima KANU00245460
836       Langloisia setosissima        Langloisia setosissima KANU361666
837       Langloisia setosissima        Langloisia setosissima KSC0126431
838       Langloisia setosissima        Langloisia setosissima KANU361666
839       Langloisia setosissima        Langloisia setosissima KSC0126435
840       Langloisia setosissima      Langloisia setosissima KANU00245460
841       Langloisia setosissima      Langloisia setosissima KANU00245460
842       Langloisia setosissima      Langloisia setosissima KANU00245459
843       Langloisia setosissima        Langloisia setosissima KANU361666
844        Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249021
845        Leptosiphon, rosaceus         Leptosiphon, rosaceus KANU365874
846        Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249023
847        Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249021
848        Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249023
849        Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249021
850        Leptosiphon, rosaceus         Leptosiphon, rosaceus KANU365874
851        Leptosiphon, rosaceus         Leptosiphon, rosaceus KANU365874
852        Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249023
853          Leptosiphon bicolor         Leptosiphon bicolor KANU00249042
854          Leptosiphon bicolor         Leptosiphon bicolor KANU00305479
855          Leptosiphon bicolor         Leptosiphon bicolor KANU00249042
856          Leptosiphon bicolor         Leptosiphon bicolor KANU00249041
857          Leptosiphon bicolor         Leptosiphon bicolor KANU00249039
858          Leptosiphon bicolor         Leptosiphon bicolor KANU00249039
859          Leptosiphon bicolor         Leptosiphon bicolor KANU00249039
860          Leptosiphon bicolor         Leptosiphon bicolor KANU00249041
861          Leptosiphon bicolor         Leptosiphon bicolor KANU00249041
862          Leptosiphon bicolor         Leptosiphon bicolor KANU00249042
863          Leptosiphon bicolor         Leptosiphon bicolor KANU00305479
864          Leptosiphon bicolor         Leptosiphon bicolor KANU00305479
865       Leptosiphon breviculus      Leptosiphon breviculus KANU00249047
866       Leptosiphon breviculus      Leptosiphon breviculus KANU00249047
867       Leptosiphon breviculus      Leptosiphon breviculus KANU00249047
868      Leptosiphon floribundus       Leptosiphon floribundus KSC0126268
869      Leptosiphon floribundus       Leptosiphon floribundus KSC0126268
870      Leptosiphon floribundus       Leptosiphon floribundus KSC0126268
871     Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249065
872     Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249065
873     Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249066
874     Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249065
875     Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249066
876     Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249066
877       Leptosiphon harknessii        Leptosiphon harknessii KANU365232
878       Leptosiphon harknessii        Leptosiphon harknessii KANU365215
879       Leptosiphon harknessii        Leptosiphon harknessii KANU365215
880       Leptosiphon harknessii        Leptosiphon harknessii KANU365232
881       Leptosiphon harknessii        Leptosiphon harknessii KANU365232
882       Leptosiphon harknessii        Leptosiphon harknessii KANU365215
883       Leptosiphon liniflorus      Leptosiphon liniflorus KANU00249080
884       Leptosiphon liniflorus      Leptosiphon liniflorus KANU00249080
885       Leptosiphon liniflorus      Leptosiphon liniflorus KANU00249080
886        Leptosiphon nuttallii            Leptosiphon nuttallii UF71467
887        Leptosiphon nuttallii            Leptosiphon nuttallii UF71467
888        Leptosiphon nuttallii         Leptosiphon nuttallii KANU365092
889        Leptosiphon nuttallii       Leptosiphon nuttallii KANU00249084
890        Leptosiphon nuttallii            Leptosiphon nuttallii UF71467
891        Leptosiphon nuttallii         Leptosiphon nuttallii KANU365092
892        Leptosiphon nuttallii       Leptosiphon nuttallii KANU00249084
893        Leptosiphon nuttallii       Leptosiphon nuttallii KANU00249084
894        Leptosiphon nuttallii            Leptosiphon nuttallii UF85671
895        Leptosiphon nuttallii            Leptosiphon nuttallii UF85671
896        Leptosiphon nuttallii         Leptosiphon nuttallii KANU365092
897        Leptosiphon nuttallii            Leptosiphon nuttallii UF85671
898      Leptosiphon parviflorus     Leptosiphon parviflorus KANU00237584
899      Leptosiphon parviflorus     Leptosiphon parviflorus KANU00249024
900      Leptosiphon parviflorus     Leptosiphon parviflorus KANU00249024
901      Leptosiphon parviflorus     Leptosiphon parviflorus KANU00237584
902      Leptosiphon parviflorus     Leptosiphon parviflorus KANU00249024
903      Leptosiphon parviflorus     Leptosiphon parviflorus KANU00237584
904  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00315954
905  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00249105
906  Leptosiphon septentrionalis   Leptosiphon septentrionalis KANU365812
907  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00249105
908  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00319340
909  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00315954
910  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00319340
911  Leptosiphon septentrionalis   Leptosiphon septentrionalis KANU365812
912  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00319340
913  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00249105
914  Leptosiphon septentrionalis   Leptosiphon septentrionalis KANU365812
915  Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00315954
916       Linanthus californicus        Linanthus californicus KSC0126439
917       Linanthus californicus        Linanthus californicus KSC0126439
918       Linanthus californicus      Linanthus californicus KANU00248052
919       Linanthus californicus         Linanthus californicus MO5333712
920       Linanthus californicus         Linanthus californicus MO3193468
921       Linanthus californicus         Linanthus californicus MO5333712
922       Linanthus californicus         Linanthus californicus MO5333712
923       Linanthus californicus           Linanthus californicus 5206791
924       Linanthus californicus      Linanthus californicus KANU00316478
925       Linanthus californicus      Linanthus californicus KANU00248052
926       Linanthus californicus           Linanthus californicus 5206791
927       Linanthus californicus           Linanthus californicus 5206791
928       Linanthus californicus         Linanthus californicus MO3193468
929       Linanthus californicus         Linanthus californicus MO3193468
930       Linanthus californicus        Linanthus californicus KSC0126439
931       Linanthus californicus      Linanthus californicus KANU00316478
932       Linanthus californicus      Linanthus californicus KANU00316478
933       Linanthus californicus      Linanthus californicus KANU00248052
934      Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126232
935      Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126239
936      Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126239
937      Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126232
938      Linanthus dianthiflorus     Linanthus dianthiflorus KANU00249056
939      Linanthus dianthiflorus     Linanthus dianthiflorus KANU00249056
940      Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126239
941      Linanthus dianthiflorus     Linanthus dianthiflorus KANU00249056
942      Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126232
943         Linanthus dichotomus          Linanthus dichotomus KANU363552
944         Linanthus dichotomus          Linanthus dichotomus KANU363552
945         Linanthus dichotomus          Linanthus dichotomus KANU363504
946         Linanthus dichotomus          Linanthus dichotomus KSC0126234
947         Linanthus dichotomus          Linanthus dichotomus KSC0126234
948         Linanthus dichotomus          Linanthus dichotomus KSC0126234
949         Linanthus dichotomus          Linanthus dichotomus KANU363519
950         Linanthus dichotomus          Linanthus dichotomus KANU363519
951         Linanthus dichotomus          Linanthus dichotomus KANU363504
952         Linanthus dichotomus          Linanthus dichotomus KANU363504
953         Linanthus dichotomus          Linanthus dichotomus KANU363552
954         Linanthus dichotomus          Linanthus dichotomus KANU363519
955            Linanthus parryae             Linanthus parryae KSC0126158
956            Linanthus parryae           Linanthus parryae KANU00249098
957            Linanthus parryae           Linanthus parryae KANU00249098
958            Linanthus parryae           Linanthus parryae KANU00249098
959            Linanthus parryae             Linanthus parryae KSC0126158
960            Linanthus parryae             Linanthus parryae KSC0126158
961            Loeselia mexicana           Loeselia mexicana KANU00256196
962            Loeselia mexicana           Loeselia mexicana KANU00256196
963            Loeselia mexicana           Loeselia mexicana KANU00256196
964      Loeseliastrum depressum      Loeseliastrum depressum RSABG503663
965      Loeseliastrum depressum      Loeseliastrum depressum RSABG782466
966      Loeseliastrum depressum      Loeseliastrum depressum RSABG782466
967      Loeseliastrum depressum      Loeseliastrum depressum RSABG503663
968      Loeseliastrum depressum      Loeseliastrum depressum RSABG782466
969      Loeseliastrum depressum      Loeseliastrum depressum RSABG503663
970     Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245450
971     Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245450
972     Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245450
973     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126432
974     Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245451
975     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126432
976     Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245451
977     Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245451
978     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126436
979     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126433
980     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126436
981     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126436
982     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126432
983     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126433
984     Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126433
985       Microgilia minutiflora      Microgilia minutiflora KANU00242198
986       Microgilia minutiflora      Microgilia minutiflora KANU00242198
987       Microgilia minutiflora      Microgilia minutiflora KANU00242198
988         Microsteris gracilis          Microsteris gracilis KSC0126260
989         Microsteris gracilis        Microsteris gracilis KANU00265609
990         Microsteris gracilis        Microsteris gracilis KANU00265630
991         Microsteris gracilis          Microsteris gracilis KSC0126256
992         Microsteris gracilis          Microsteris gracilis KSC0126260
993         Microsteris gracilis          Microsteris gracilis KSC0126256
994         Microsteris gracilis        Microsteris gracilis KANU00265630
995         Microsteris gracilis          Microsteris gracilis KSC0126256
996         Microsteris gracilis        Microsteris gracilis KANU00312371
997         Microsteris gracilis        Microsteris gracilis KANU00312371
998         Microsteris gracilis        Microsteris gracilis KANU00312371
999         Microsteris gracilis        Microsteris gracilis KANU00265609
1000        Microsteris gracilis        Microsteris gracilis KANU00265630
1001        Microsteris gracilis        Microsteris gracilis KANU00265609
1002    Navarretia atractyloides    Navarretia atractyloides KANU00257719
1003    Navarretia atractyloides    Navarretia atractyloides KANU00257719
1004    Navarretia atractyloides    Navarretia atractyloides KANU00257719
1005       Navarretia capillaris         Navarretia capillaris KANU363792
1006       Navarretia capillaris         Navarretia capillaris KANU363792
1007       Navarretia capillaris         Navarretia capillaris KANU363792
1008       Navarretia capillaris       Navarretia capillaris KANU00237614
1009       Navarretia capillaris       Navarretia capillaris KANU00237614
1010       Navarretia capillaris       Navarretia capillaris KANU00237614
1011           Navarretia hamata           Navarretia hamata KANU00257734
1012           Navarretia hamata           Navarretia hamata KANU00257734
1013         Navarretia leptalea            Navarretia leptalea FLAS72028
1014         Navarretia leptalea              Navarretia leptalea UF72028
1015         Navarretia leptalea         Navarretia leptalea KANU00237681
1016         Navarretia leptalea         Navarretia leptalea KANU00237681
1017         Navarretia leptalea            Navarretia leptalea FLAS72028
1018         Navarretia leptalea         Navarretia leptalea KANU00237682
1019         Navarretia leptalea              Navarretia leptalea UF72028
1020         Navarretia leptalea         Navarretia leptalea KANU00237682
1021         Navarretia leptalea              Navarretia leptalea UF72028
1022         Navarretia leptalea         Navarretia leptalea KANU00237682
1023         Navarretia leptalea         Navarretia leptalea KANU00237681
1024         Navarretia leptalea            Navarretia leptalea FLAS72028
1025     Navarretia peninsularis        Navarretia peninsularis MO3206031
1026     Navarretia peninsularis        Navarretia peninsularis MO3206031
1027     Navarretia peninsularis        Navarretia peninsularis MO3206031
1028        Navarretia pubescens        Navarretia pubescens KANU00329164
1029        Navarretia pubescens        Navarretia pubescens KANU00329164
1030        Navarretia pubescens        Navarretia pubescens KANU00329164
1031        Navarretia pubescens        Navarretia pubescens KANU00257768
1032        Navarretia pubescens        Navarretia pubescens KANU00257768
1033        Navarretia pubescens        Navarretia pubescens KANU00257768
1034        Navarretia squarrosa        Navarretia squarrosa KANU00257775
1035        Navarretia squarrosa        Navarretia squarrosa KANU00257775
1036        Navarretia squarrosa        Navarretia squarrosa KANU00257738
1037        Navarretia squarrosa        Navarretia squarrosa KANU00314368
1038        Navarretia squarrosa        Navarretia squarrosa KANU00314368
1039        Navarretia squarrosa        Navarretia squarrosa KANU00314368
1040        Navarretia squarrosa        Navarretia squarrosa KANU00257738
1041        Navarretia squarrosa        Navarretia squarrosa KANU00257738
1042        Navarretia squarrosa        Navarretia squarrosa KANU00257737
1043        Navarretia squarrosa        Navarretia squarrosa KANU00257737
1044        Navarretia squarrosa        Navarretia squarrosa KANU00257775
1045       Navarretia subuligera          Navarretia subuligera MO1243591
1046       Navarretia subuligera         Navarretia subuligera MO05088307
1047       Navarretia subuligera          Navarretia subuligera MO1243591
1048       Navarretia subuligera         Navarretia subuligera MO05088307
1049        Navarretia viscidula        Navarretia viscidula KANU00257782
1050        Navarretia viscidula        Navarretia viscidula KANU00257782
1051        Navarretia viscidula        Navarretia viscidula KANU00257782
1052        Navarretia viscidula        Navarretia viscidula KANU00257781
1053        Navarretia viscidula        Navarretia viscidula KANU00257781
1054        Navarretia viscidula        Navarretia viscidula KANU00257781
1055                Phlox, icola                Phlox, icola KANU00265362
1056                Phlox, icola                Phlox, icola KANU00265366
1057                Phlox, icola                Phlox, icola KANU00330422
1058                Phlox, icola                Phlox, icola KANU00265366
1059                Phlox, icola                Phlox, icola KANU00265362
1060                Phlox, icola                Phlox, icola KANU00265362
1061                Phlox, icola                Phlox, icola KANU00265366
1062                Phlox, icola                Phlox, icola KANU00330422
1063                Phlox, icola                Phlox, icola KANU00330422
1064            Phlox caespitosa            Phlox caespitosa KANU00265455
1065            Phlox caespitosa            Phlox caespitosa KANU00265448
1066            Phlox caespitosa            Phlox caespitosa KANU00265455
1067            Phlox caespitosa            Phlox caespitosa KANU00265448
1068            Phlox caespitosa            Phlox caespitosa KANU00265448
1069            Phlox caespitosa            Phlox caespitosa KANU00265455
1070             Phlox cuspidata               Phlox cuspidata KSC0129916
1071             Phlox cuspidata               Phlox cuspidata KSC0129915
1072             Phlox cuspidata               Phlox cuspidata KSC0129917
1073             Phlox cuspidata               Phlox cuspidata KSC0129917
1074             Phlox cuspidata               Phlox cuspidata KSC0129915
1075             Phlox cuspidata             Phlox cuspidata KANU00265460
1076             Phlox cuspidata             Phlox cuspidata KANU00265458
1077             Phlox cuspidata               Phlox cuspidata KSC0129916
1078             Phlox cuspidata             Phlox cuspidata KANU00265458
1079             Phlox cuspidata               Phlox cuspidata KSC0129916
1080             Phlox cuspidata             Phlox cuspidata KANU00265458
1081             Phlox cuspidata               Phlox cuspidata KSC0129917
1082             Phlox cuspidata               Phlox cuspidata KSC0129915
1083             Phlox cuspidata             Phlox cuspidata KANU00265460
1084             Phlox cuspidata             Phlox cuspidata KANU00265460
1085               Phlox diffusa               Phlox diffusa KANU00265473
1086               Phlox diffusa               Phlox diffusa KANU00265469
1087               Phlox diffusa               Phlox diffusa KANU00265469
1088               Phlox diffusa               Phlox diffusa KANU00265473
1089               Phlox diffusa               Phlox diffusa KANU00265472
1090               Phlox diffusa               Phlox diffusa KANU00265473
1091               Phlox diffusa               Phlox diffusa KANU00265472
1092               Phlox diffusa               Phlox diffusa KANU00265472
1093               Phlox diffusa               Phlox diffusa KANU00265469
1094            Phlox divaricata              Phlox divaricata KANU348246
1095            Phlox divaricata              Phlox divaricata KSC0129933
1096            Phlox divaricata              Phlox divaricata KSC0129933
1097            Phlox divaricata              Phlox divaricata KANU348246
1098            Phlox divaricata              Phlox divaricata KANU348246
1099            Phlox divaricata              Phlox divaricata KANU349429
1100            Phlox divaricata              Phlox divaricata KANU349429
1101            Phlox divaricata              Phlox divaricata KSC0129959
1102            Phlox divaricata              Phlox divaricata KSC0129934
1103            Phlox divaricata              Phlox divaricata KSC0129934
1104            Phlox divaricata              Phlox divaricata KSC0129934
1105            Phlox divaricata              Phlox divaricata KSC0129933
1106            Phlox divaricata              Phlox divaricata KANU349429
1107            Phlox divaricata              Phlox divaricata KANU349877
1108            Phlox divaricata              Phlox divaricata KANU349877
1109            Phlox divaricata              Phlox divaricata KSC0129959
1110            Phlox divaricata              Phlox divaricata KANU349877
1111            Phlox divaricata              Phlox divaricata KSC0129959
1112            Phlox divaricata                    Phlox divaricata KSC2
1113            Phlox divaricata                     Phlox divaricata KSC
1114            Phlox divaricata                     Phlox divaricata KSC
1115            Phlox divaricata                     Phlox divaricata KSC
1116            Phlox divaricata                    Phlox divaricata KSC2
1117           Phlox dolichantha             Phlox dolichantha KSC0129993
1118           Phlox dolichantha             Phlox dolichantha KSC0129993
1119           Phlox dolichantha              Phlox dolichantha KSC012992
1120           Phlox dolichantha              Phlox dolichantha KSC012992
1121           Phlox dolichantha             Phlox dolichantha KSC0129993
1122           Phlox dolichantha              Phlox dolichantha KSC012992
1123            Phlox glaberrima              Phlox glaberrima KSC0130091
1124            Phlox glaberrima              Phlox glaberrima KSC0130094
1125            Phlox glaberrima            Phlox glaberrima KANU00265598
1126            Phlox glaberrima            Phlox glaberrima KANU00265598
1127            Phlox glaberrima              Phlox glaberrima KSC0130091
1128            Phlox glaberrima              Phlox glaberrima KSC0130091
1129            Phlox glaberrima              Phlox glaberrima KSC0130094
1130            Phlox glaberrima              Phlox glaberrima KSC0130094
1131            Phlox glaberrima            Phlox glaberrima KANU00265599
1132            Phlox glaberrima              Phlox glaberrima KSC0130086
1133            Phlox glaberrima            Phlox glaberrima KANU00265599
1134            Phlox glaberrima            Phlox glaberrima KANU00265598
1135            Phlox glaberrima              Phlox glaberrima KSC0130086
1136            Phlox glaberrima            Phlox glaberrima KANU00265599
1137            Phlox glaberrima              Phlox glaberrima KSC0130086
1138              Phlox maculata              Phlox maculata KANU00265733
1139              Phlox maculata              Phlox maculata KANU00265723
1140              Phlox maculata                Phlox maculata KSC0130160
1141              Phlox maculata              Phlox maculata KANU00265733
1142              Phlox maculata                Phlox maculata KSC0130160
1143              Phlox maculata              Phlox maculata KANU00265723
1144              Phlox maculata                Phlox maculata KSC0130160
1145              Phlox maculata              Phlox maculata KANU00265723
1146              Phlox maculata              Phlox maculata KANU00265733
1147              Phlox maculata                Phlox maculata KSC0130153
1148              Phlox maculata                Phlox maculata KSC0130156
1149              Phlox maculata                Phlox maculata KSC0130153
1150              Phlox maculata                Phlox maculata KSC0130156
1151              Phlox maculata                Phlox maculata KSC0130156
1152              Phlox maculata                Phlox maculata KSC0130153
1153            Phlox multiflora            Phlox multiflora KANU00322730
1154            Phlox multiflora            Phlox multiflora KANU00324002
1155            Phlox multiflora            Phlox multiflora KANU00322730
1156            Phlox multiflora            Phlox multiflora KANU00265737
1157            Phlox multiflora            Phlox multiflora KANU00324002
1158            Phlox multiflora            Phlox multiflora KANU00324002
1159            Phlox multiflora            Phlox multiflora KANU00265737
1160            Phlox multiflora            Phlox multiflora KANU00265737
1161                  Phlox nana                  Phlox nana KANU00265768
1162                  Phlox nana                  Phlox nana KANU00265772
1163                  Phlox nana                  Phlox nana KANU00265770
1164                  Phlox nana                  Phlox nana KANU00265768
1165                  Phlox nana                  Phlox nana KANU00265770
1166                  Phlox nana                  Phlox nana KANU00265770
1167                  Phlox nana                  Phlox nana KANU00265772
1168                  Phlox nana                  Phlox nana KANU00265772
1169                  Phlox nana                  Phlox nana KANU00265768
1170            Phlox paniculata            Phlox paniculata KANU00078424
1171            Phlox paniculata            Phlox paniculata KANU00078426
1172            Phlox paniculata            Phlox paniculata KANU00078426
1173            Phlox paniculata            Phlox paniculata KANU00078426
1174            Phlox paniculata              Phlox paniculata KANU353051
1175            Phlox paniculata              Phlox paniculata KANU353051
1176            Phlox paniculata              Phlox paniculata KSC0130191
1177            Phlox paniculata              Phlox paniculata KSC0130191
1178            Phlox paniculata              Phlox paniculata KSC0130191
1179            Phlox paniculata            Phlox paniculata KANU00078424
1180            Phlox paniculata              Phlox paniculata KSC0130189
1181            Phlox paniculata              Phlox paniculata KSC0130188
1182            Phlox paniculata              Phlox paniculata KSC0130188
1183            Phlox paniculata            Phlox paniculata KANU00078424
1184            Phlox paniculata              Phlox paniculata KSC0130189
1185            Phlox paniculata              Phlox paniculata KANU353051
1186            Phlox paniculata              Phlox paniculata KSC0130188
1187            Phlox paniculata              Phlox paniculata KSC0130189
1188                Phlox pilosa                  Phlox pilosa KSC0130223
1189                Phlox pilosa                  Phlox pilosa KSC0130228
1190                Phlox pilosa                  Phlox pilosa KSC0130223
1191                Phlox pilosa                  Phlox pilosa KSC0130205
1192                Phlox pilosa                  Phlox pilosa KSC0130205
1193                Phlox pilosa                  Phlox pilosa KSC0130223
1194                Phlox pilosa                  Phlox pilosa KSC0130228
1195                Phlox pilosa                  Phlox pilosa KSC0130205
1196            Phlox roemeriana            Phlox roemeriana KANU00265947
1197            Phlox roemeriana            Phlox roemeriana KANU00265950
1198            Phlox roemeriana            Phlox roemeriana KANU00265950
1199            Phlox roemeriana            Phlox roemeriana KANU00265946
1200            Phlox roemeriana            Phlox roemeriana KANU00265946
1201            Phlox roemeriana            Phlox roemeriana KANU00265947
1202            Phlox roemeriana            Phlox roemeriana KANU00265947
1203            Phlox roemeriana            Phlox roemeriana KANU00265950
1204            Phlox roemeriana            Phlox roemeriana KANU00265946
1205            Phlox stansburyi            Phlox stansburyi KANU00265960
1206            Phlox stansburyi            Phlox stansburyi KANU00265960
1207            Phlox stansburyi            Phlox stansburyi KANU00265960
1208            Phlox stansburyi            Phlox stansburyi KANU00265962
1209            Phlox stansburyi            Phlox stansburyi KANU00265962
1210            Phlox stansburyi            Phlox stansburyi KANU00265962
1211           Phlox stolonifera           Phlox stolonifera KANU00265964
1212           Phlox stolonifera             Phlox stolonifera KSC0130332
1213           Phlox stolonifera           Phlox stolonifera KANU00324226
1214           Phlox stolonifera           Phlox stolonifera KANU00324226
1215           Phlox stolonifera           Phlox stolonifera KANU00265964
1216           Phlox stolonifera             Phlox stolonifera KSC0130330
1217           Phlox stolonifera             Phlox stolonifera KSC0130330
1218           Phlox stolonifera           Phlox stolonifera KANU00265964
1219           Phlox stolonifera             Phlox stolonifera KSC0130332
1220           Phlox stolonifera             Phlox stolonifera KSC0130332
1221           Phlox stolonifera           Phlox stolonifera KANU00324226
1222           Phlox stolonifera             Phlox stolonifera KSC0130331
1223              Phlox subulata              Phlox subulata KANU00265968
1224              Phlox subulata              Phlox subulata KANU00265966
1225              Phlox subulata                Phlox subulata KSC0130334
1226              Phlox subulata              Phlox subulata KANU00265966
1227              Phlox subulata              Phlox subulata KANU00265973
1228              Phlox subulata              Phlox subulata KANU00265973
1229              Phlox subulata              Phlox subulata KANU00265968
1230              Phlox subulata              Phlox subulata KANU00265968
1231              Phlox subulata                Phlox subulata KSC0130333
1232              Phlox subulata                Phlox subulata KSC0130336
1233              Phlox subulata                Phlox subulata KSC0130336
1234              Phlox subulata                Phlox subulata KSC0130336
1235              Phlox subulata              Phlox subulata KANU00265974
1236              Phlox subulata              Phlox subulata KANU00265974
1237              Phlox subulata              Phlox subulata KANU00265973
1238              Phlox subulata              Phlox subulata KANU00265974
1239              Phlox subulata              Phlox subulata KANU00265966
1240              Phlox subulata                Phlox subulata KSC0130333
1241              Phlox subulata                Phlox subulata KSC0130333
1242          Polemonium boreale          Polemonium boreale KANU00263983
1243          Polemonium boreale          Polemonium boreale KANU00263983
1244          Polemonium boreale          Polemonium boreale KANU00263983
1245        Polemonium caeruleum        Polemonium caeruleum KANU00263984
1246        Polemonium caeruleum        Polemonium caeruleum KANU00263984
1247     Polemonium californicum     Polemonium californicum KANU00263987
1248     Polemonium californicum     Polemonium californicum KANU00263987
1249     Polemonium californicum     Polemonium californicum KANU00263988
1250     Polemonium californicum     Polemonium californicum KANU00263987
1251     Polemonium californicum     Polemonium californicum KANU00263988
1252     Polemonium californicum       Polemonium californicum KSC0130367
1253     Polemonium californicum       Polemonium californicum KSC0130367
1254     Polemonium californicum     Polemonium californicum KANU00263988
1255     Polemonium californicum       Polemonium californicum KSC0130367
1256        Polemonium confertum        Polemonium confertum KANU00263989
1257        Polemonium confertum        Polemonium confertum KANU00263989
1258        Polemonium confertum          Polemonium confertum KSC0130369
1259        Polemonium confertum          Polemonium confertum KSC0130369
1260        Polemonium confertum           Polemonium confertum RMH736397
1261        Polemonium confertum           Polemonium confertum RMH776743
1262        Polemonium confertum           Polemonium confertum RMH776743
1263        Polemonium confertum           Polemonium confertum RMH721962
1264        Polemonium confertum           Polemonium confertum RMH721974
1265        Polemonium confertum           Polemonium confertum RMH721962
1266        Polemonium confertum           Polemonium confertum RMH721962
1267        Polemonium confertum           Polemonium confertum RMH736397
1268        Polemonium confertum           Polemonium confertum RMH736397
1269        Polemonium delicatum        Polemonium delicatum KANU00263992
1270        Polemonium delicatum        Polemonium delicatum KANU00263991
1271        Polemonium delicatum        Polemonium delicatum KANU00263993
1272        Polemonium delicatum        Polemonium delicatum KANU00263992
1273        Polemonium delicatum        Polemonium delicatum KANU00263991
1274        Polemonium delicatum        Polemonium delicatum KANU00263993
1275        Polemonium delicatum        Polemonium delicatum KANU00263993
1276        Polemonium delicatum        Polemonium delicatum KANU00263992
1277          Polemonium elegans             Polemonium elegans MO1012176
1278          Polemonium elegans             Polemonium elegans MO1012176
1279          Polemonium elegans              Polemonium elegans MO991425
1280          Polemonium elegans             Polemonium elegans MO2751520
1281          Polemonium elegans             Polemonium elegans MO2751520
1282          Polemonium elegans              Polemonium elegans MO991425
1283          Polemonium elegans              Polemonium elegans MO991425
1284          Polemonium elegans             Polemonium elegans MO2751520
1285          Polemonium elegans             Polemonium elegans MO1012176
1286          Polemonium eximium             Polemonium eximium MO2751795
1287          Polemonium eximium             Polemonium eximium MO2751795
1288          Polemonium eximium             Polemonium eximium MO2751797
1289    Polemonium foliosissimum    Polemonium foliosissimum KANU00268633
1290    Polemonium foliosissimum      Polemonium foliosissimum KSC0130375
1291    Polemonium foliosissimum      Polemonium foliosissimum KSC0130375
1292    Polemonium foliosissimum      Polemonium foliosissimum KSC0130374
1293    Polemonium foliosissimum      Polemonium foliosissimum KSC0130376
1294    Polemonium foliosissimum      Polemonium foliosissimum KSC0130376
1295    Polemonium foliosissimum      Polemonium foliosissimum KSC0130375
1296    Polemonium foliosissimum      Polemonium foliosissimum KSC0130374
1297    Polemonium foliosissimum      Polemonium foliosissimum KSC0130374
1298    Polemonium foliosissimum    Polemonium foliosissimum KANU00268634
1299    Polemonium foliosissimum    Polemonium foliosissimum KANU00268633
1300    Polemonium foliosissimum    Polemonium foliosissimum KANU00268633
1301    Polemonium foliosissimum    Polemonium foliosissimum KANU00268634
1302    Polemonium foliosissimum    Polemonium foliosissimum KANU00335551
1303    Polemonium foliosissimum    Polemonium foliosissimum KANU00268634
1304    Polemonium foliosissimum    Polemonium foliosissimum KANU00335551
1305    Polemonium foliosissimum    Polemonium foliosissimum KANU00335551
1306    Polemonium foliosissimum      Polemonium foliosissimum KSC0130371
1307    Polemonium foliosissimum      Polemonium foliosissimum KSC0130371
1308    Polemonium foliosissimum      Polemonium foliosissimum KSC0130371
1309        Polemonium mexicanum           Polemonium mexicanum MO1736468
1310        Polemonium mexicanum           Polemonium mexicanum MO1129129
1311        Polemonium mexicanum           Polemonium mexicanum MO1129129
1312        Polemonium mexicanum           Polemonium mexicanum MO1736468
1313        Polemonium mexicanum           Polemonium mexicanum MO1129129
1314        Polemonium mexicanum           Polemonium mexicanum MO1736468
1315        Polemonium mexicanum           Polemonium mexicanum MO2751532
1316        Polemonium mexicanum           Polemonium mexicanum MO2751532
1317        Polemonium mexicanum           Polemonium mexicanum MO2751532
1318       Polemonium micranthum       Polemonium micranthum KANU00343808
1319       Polemonium micranthum       Polemonium micranthum KANU00314510
1320       Polemonium micranthum         Polemonium micranthum KSC0130380
1321       Polemonium micranthum         Polemonium micranthum KSC0130380
1322       Polemonium micranthum         Polemonium micranthum KSC0130379
1323       Polemonium micranthum         Polemonium micranthum KSC0130379
1324       Polemonium micranthum         Polemonium micranthum KSC0130380
1325      Polemonium pauciflorum         Polemonium pauciflorum MO1229549
1326      Polemonium pauciflorum         Polemonium pauciflorum MO2751940
1327     Polemonium pulcherrimum     Polemonium pulcherrimum KANU00331042
1328     Polemonium pulcherrimum       Polemonium pulcherrimum KSC0130389
1329     Polemonium pulcherrimum     Polemonium pulcherrimum KANU00331042
1330     Polemonium pulcherrimum       Polemonium pulcherrimum KSC0130388
1331     Polemonium pulcherrimum       Polemonium pulcherrimum KSC0130389
1332     Polemonium pulcherrimum     Polemonium pulcherrimum KANU00331042
1333          Polemonium reptans          Polemonium reptans KANU00080962
1334          Polemonium reptans          Polemonium reptans KANU00080962
1335          Polemonium reptans          Polemonium reptans KANU00311734
1336          Polemonium reptans          Polemonium reptans KANU00311734
1337          Polemonium reptans          Polemonium reptans KANU00311734
1338          Polemonium reptans            Polemonium reptans KSC0130401
1339          Polemonium reptans            Polemonium reptans KSC0130401
1340          Polemonium reptans            Polemonium reptans KSC0130404
1341          Polemonium reptans            Polemonium reptans KSC0130404
1342          Polemonium reptans            Polemonium reptans KSC0130405
1343          Polemonium reptans            Polemonium reptans KSC0130405
1344          Polemonium reptans            Polemonium reptans KSC0130403
1345          Polemonium reptans            Polemonium reptans KSC0130401
1346          Polemonium reptans            Polemonium reptans KSC0130403
1347          Polemonium reptans          Polemonium reptans KANU00080960
1348          Polemonium reptans          Polemonium reptans KANU00080960
1349          Polemonium reptans          Polemonium reptans KANU00080960
1350         Polemonium viscosum         Polemonium viscosum KANU00268697
1351         Polemonium viscosum         Polemonium viscosum KANU00268697
1352         Polemonium viscosum           Polemonium viscosum KSC0130410
1353         Polemonium viscosum           Polemonium viscosum KSC0130410
1354         Polemonium viscosum         Polemonium viscosum KANU00268698
1355         Polemonium viscosum         Polemonium viscosum KANU00268698
1356         Polemonium viscosum         Polemonium viscosum KANU00268698
1357         Polemonium viscosum         Polemonium viscosum KANU00268697
1358         Polemonium viscosum         Polemonium viscosum KANU00268704
1359         Polemonium viscosum         Polemonium viscosum KANU00268704
1360        Saltugilia australis           Saltugilia australis MO2684686
1361        Saltugilia australis         Saltugilia australis RSABG672125
1362        Saltugilia australis         Saltugilia australis RSABG599790
1363        Saltugilia australis         Saltugilia australis RSABG599790
1364        Saltugilia australis         Saltugilia australis RSABG672125
1365        Saltugilia australis         Saltugilia australis RSABG672125
1366        Saltugilia australis           Saltugilia australis MO2684686
1367        Saltugilia australis           Saltugilia australis MO2684686
1368        Saltugilia australis           Saltugilia australis MO4000368
1369        Saltugilia australis           Saltugilia australis MO3890157
1370        Saltugilia australis           Saltugilia australis MO3890157
1371        Saltugilia australis           Saltugilia australis MO3890157
1372        Saltugilia australis           Saltugilia australis MO4000368
1373        Saltugilia australis           Saltugilia australis MO4000368
1374        Saltugilia australis         Saltugilia australis RSABG599790
1375       Saltugilia caruifolia       Saltugilia caruifolia KANU00237633
1376       Saltugilia caruifolia        Saltugilia caruifolia RSABG731741
1377       Saltugilia caruifolia       Saltugilia caruifolia KANU00237633
1378       Saltugilia caruifolia       Saltugilia caruifolia KANU00237633
1379       Saltugilia caruifolia        Saltugilia caruifolia RSABG519433
1380       Saltugilia caruifolia        Saltugilia caruifolia RSABG519433
1381       Saltugilia caruifolia        Saltugilia caruifolia RSABG519433
1382       Saltugilia caruifolia        Saltugilia caruifolia RSABG731741
1383       Saltugilia caruifolia        Saltugilia caruifolia RSABG731741
     flower_number corolla_length_cm corolla_width_throat_cm     color_opt_1
1                1             3.696                   0.455           white
2                3             2.874                   0.375           white
3                1             3.552                   0.393           white
4                2              2.66                    0.37           white
5                1             1.652                   0.241             red
6                2             1.664                   0.148             red
7                3             1.885                   0.201             red
8                1             0.881                   0.172           white
9                1             0.739                   0.144           white
10               3             0.957                   0.272           white
11               3             0.609                   0.085           white
12               2             0.916                   0.161           white
13               2             0.803                   0.116           white
14               3             0.395                   0.074           white
15               3             0.519                   0.088           white
16               2             0.635                   0.115           white
17               2              0.45                   0.084           white
18               1             0.731                   0.141           white
19               1             0.508                   0.083           white
20               3             0.551                   0.114           white
21               1             0.395                   0.084           white
22               2             0.478                   0.084           white
23               2             0.213                   0.049           white
24               1             0.202                   0.038           white
25               3             0.224                   0.041           white
26               3             0.565                    0.15            blue
27               1              0.57                   0.175            blue
28               3             0.666                   0.196            blue
29               2             0.612                   0.142            blue
30               1              0.62                   0.149            blue
31               2             0.624                   0.163            blue
32               3             1.448                   0.225             red
33               2             1.503                   0.207             red
34               3             1.875                   0.277             red
35               1             1.792                   0.293             red
36               3             1.681                   0.241             red
37               2             1.355                   0.204             red
38               2             1.425                    0.23             red
39               2             1.541                   0.197             red
40               3             1.621                   0.246             red
41               2             1.781                   0.273             red
42               3             1.465                   0.254             red
43               1             1.424                    0.22             red
44               1             1.714                   0.221             red
45               2             1.328                   0.194             red
46               1             1.501                   0.194             red
47               1              1.31                   0.185             red
48               1             1.481                   0.248             red
49               3             1.592                   0.193             red
50               2             0.633                   0.106            pink
51               3             0.632                   0.111            pink
52               1             0.686                   0.199            pink
53               3             0.744                   0.102     blue-purple
54               1             0.633                   0.095     blue-purple
55               1             0.658                   0.079     blue-purple
56               3             0.274                   0.074     blue-purple
57               2             0.628                   0.084     blue-purple
58               3             0.393                   0.073     blue-purple
59               1             0.344                   0.065     blue-purple
60               2             0.261                   0.053     blue-purple
61               2             0.596                   0.078     blue-purple
62               3             0.381                   0.065     blue-purple
63               2             0.317                   0.081     blue-purple
64               3             0.399                   0.086     blue-purple
65               3             0.454                    0.07     blue-purple
66               1              0.33                   0.071     blue-purple
67               2             0.312                   0.071     blue-purple
68               2             0.467                   0.081     blue-purple
69               2             0.433                   0.082     blue-purple
70               2             0.516                   0.074     blue-purple
71               3             0.373                     0.8     blue-purple
72               1              0.31                   0.054     blue-purple
73               3             0.544                   0.062     blue-purple
74               2             0.417                   0.061     blue-purple
75               2             1.835                   0.359     blue-purple
76               3             0.499                   0.065     blue-purple
77               1             0.538                   0.071     blue-purple
78               1             2.125                   0.282     blue-purple
79               1             0.379                   0.051     blue-purple
80               1              0.72                   0.092     blue-purple
81               1             0.356                   0.068     blue-purple
82               3             2.107                    0.27     blue-purple
83               3             0.737                   0.116     blue-purple
84               1             0.566                   0.083     blue-purple
85               2             0.614                    0.11     blue-purple
86               2             0.833                   0.128     blue-purple
87               3             0.645                   0.128     blue-purple
88               1             0.715                   0.122     blue-purple
89               3             0.586                   0.239           white
90               2              0.44                   0.115           white
91               3             0.503                   0.135           white
92               2             0.555                   0.191           white
93               3             0.385                    0.11           white
94               1             0.449                    0.11           white
95               1             0.429                   0.085           white
96               1             0.409                   0.111           white
97               2              0.35                    0.08           white
98               2              0.39                   0.093            blue
99               2             0.671                   0.212            blue
100              3             0.335                   0.088            blue
101              1             0.347                   0.083            blue
102              1             0.304                   0.106            blue
103              3             0.339                   0.073            blue
104              2             0.347                   0.093            blue
105              3             0.567                   0.238            blue
106              1             0.592                   0.244            blue
107              2             1.791                   0.255     blue-purple
108              2             1.278                   0.292     blue-purple
109              1             1.893                   0.212     blue-purple
110              1             1.846                   0.167     blue-purple
111              3             1.518                   0.201     blue-purple
112              3             1.263                   0.276     blue-purple
113              3             7.206                   0.889             red
114              3             6.892                   1.167             red
115              1             6.963                   0.901             red
116              2             7.007                    1.22             red
117              2             6.333                   0.983             red
118              3             4.593                   0.701             red
119              1             7.162                   0.943             red
120              1             6.379                   1.057             red
121              2             7.216                   0.904             red
122              2              5.56                   0.782             red
123              1             5.561                   0.792             red
124              2             5.686                   0.783             red
125              1              5.15                   0.955             red
126              3             5.995                    0.87             red
127              3             6.168                   1.003             red
128              1             3.168                   0.634           white
129              1              3.07                   0.448           white
130              2              3.94                   0.552           white
131              3             3.865                    0.56           white
132              2             3.248                   0.441           white
133              3             3.231                   0.531           white
134              2              2.81                   0.346           white
135              1              3.03                   0.447           white
136              1             2.209                    1.99           green
137              1             5.936                   1.356           green
138              1             3.677                   1.625           green
139              1             2.522                   1.465 greenish-yellow
140              1             5.465                   2.281 greenish-yellow
141              1              5.54                   2.256 greenish-yellow
142              1             6.199                   2.988 greenish-yellow
143              1             5.559                    2.75 greenish-yellow
144              1             4.769                   1.519 greenish-yellow
145              1             5.015                   2.712 greenish-yellow
146              1             5.211                   2.459 greenish-yellow
147              1             5.549                   2.545 greenish-yellow
148              1             2.945                   1.653 greenish-yellow
149              1             4.914                   2.748 greenish-yellow
150              1             4.117                   1.976           green
151              1             4.828                   2.232           green
152              1             4.102                   1.922           green
153              1             5.574                   0.606           green
154              1             4.948                   0.605           green
155              2             5.602                    2.24          yellow
156              1             5.715                   2.403          yellow
157              2             4.571                   4.459          purple
158              3             3.686                   3.091          purple
159              1              4.52                    3.51          purple
160              1             5.795                   3.064          purple
161              2             5.784                   3.288            pink
162              1             6.239                   3.772            pink
163              1              6.03                   3.691            pink
164              1               6.2                   3.226            pink
165              1             2.026                    0.38          yellow
166              2             1.004                   0.364          yellow
167              2             1.571                   0.138          yellow
168              2             1.393                   0.315          yellow
169              3             1.864                   0.159          yellow
170              2             2.137                   0.388          yellow
171              1             1.052                   0.199          yellow
172              1             2.104                   0.306          yellow
173              3             1.164                   0.306          yellow
174              1             1.173                   0.334          yellow
175              2             1.192                     0.3          yellow
176              1             1.523                   0.276          yellow
177              1             1.441                   0.258          yellow
178              3             1.579                   0.212          yellow
179              1             2.018                   0.268          yellow
180              3              1.04                   0.251          yellow
181              3             1.247                   0.333          yellow
182              1             1.306                   0.357          yellow
183              3             1.482                   0.239          yellow
184              3             1.842                   0.369          yellow
185              3               1.6                   0.372          yellow
186              2             1.878                   0.213          yellow
187              1             1.632                   0.318          yellow
188              2             1.399                   0.164          yellow
189              2             1.212                   0.516          yellow
190              1             0.812                   0.119       pale pink
191              1             1.265                   0.211       pale pink
192              1             0.765                   0.143       pale pink
193              3             0.806                   0.099       pale pink
194              3             1.135                   0.171       pale pink
195              3             0.569                   0.056       pale pink
196              2             1.033                   0.172       pale pink
197              3             1.291                   0.154       pale pink
198              3             0.798                   0.091       pale pink
199              1             1.023                   0.193       pale pink
200              2             0.895                   0.145       pale pink
201              2              0.88                    0.09       pale pink
202              1             1.193                   0.113       pale pink
203              3             0.883                   0.118       pale pink
204              1             0.561                   0.064       pale pink
205              3              1.06                   0.099       pale pink
206              2             0.731                   0.126       pale pink
207              1             1.195                   0.184       pale pink
208              2             1.221                   0.205       pale pink
209              1             0.794                   0.122       pale pink
210              2             0.941                   0.129       pale pink
211              2             0.779                   0.121       pale pink
212              2             0.972                   0.187       pale pink
213              2             0.881                   0.104       pale pink
214              3             0.936                   0.147       pale pink
215              2             0.801                   0.148       pale pink
216              2             0.655                   0.103       pale pink
217              3             0.768                   0.134       pale pink
218              3             0.647                    0.09       pale pink
219              3             0.824                   0.118       pale pink
220              1             0.632                   0.124       pale pink
221              1             0.927                   0.145       pale pink
222              3             0.897                   0.088       pale pink
223              2             0.455                   0.061       pale pink
224              1             0.772                   0.077       pale pink
225              1             0.823                    0.15       pale pink
226              2             0.694                   0.116       pale pink
227              1             0.971                   0.112       pale pink
228              2             0.434                   0.095          purple
229              3             0.453                   0.108          purple
230              3             0.465                   0.097          purple
231              3             0.594                    0.09          purple
232              1              0.54                   0.115          purple
233              3             0.679                   0.127          purple
234              1             0.484                   0.094          purple
235              2             0.579                   0.097          purple
236              1             0.682                   0.176          purple
237              1             0.565                   0.101          purple
238              1             0.662                    0.12          purple
239              3             0.599                   0.086          purple
240              2             0.692                   0.119          purple
241              2             0.467                   0.109          purple
242              2             0.492                   0.069          purple
243              2             0.497                   0.113          purple
244              2             0.806                   0.092          purple
245              1              0.42                   0.095          purple
246              2             0.544                   0.119          purple
247              1             0.801                   0.133          purple
248              3             0.492                    0.15          purple
249              1             0.357                   0.052          purple
250              1             0.559                   0.085          purple
251              1             0.645                   0.147          purple
252              2             0.459                    0.09          purple
253              2             0.654                   0.147          purple
254              2             0.441                   0.095          purple
255              1             0.564                   0.141          purple
256              2             0.576                   0.085          purple
257              2             0.581                   0.098          purple
258              3             0.629                   0.181          purple
259              3               0.5                   0.112          purple
260              1             0.585                   0.117          purple
261              1             0.764                   0.112          purple
262              3             0.533                   0.119          purple
263              3             0.466                   0.094          purple
264              3             0.281                   0.042          purple
265              3             0.441                   0.139          purple
266              3             0.582                   0.119          purple
267              3             1.277                   0.326     purple-blue
268              1             1.409                   0.406     purple-blue
269              2             1.467                   0.288     purple-blue
270              2             1.365                   0.306     purple-blue
271              1             1.093                   0.313     purple-blue
272              3             1.239                   0.364     purple-blue
273              3             2.335                   0.452             red
274              2             2.325                    0.46             red
275              2             2.627                   0.528             red
276              1             2.164                   0.434             red
277              2             3.116                   0.452             red
278              1             3.159                   0.617             red
279              1             2.433                   0.462             red
280              3             3.105                   0.516             red
281              3             2.774                   0.461             red
282              1             0.434                    0.09          purple
283              2             0.285                   0.087          purple
284              3             0.434                   0.087          purple
285              3             0.615                   0.061          purple
286              2              0.64                   0.083          purple
287              1             0.705                   0.081          purple
288              1             0.743                   0.074          purple
289              3             0.722                   0.071          purple
290              2             0.653                   0.084          purple
291              2             1.347                   0.523            blue
292              1             1.403                   0.279            blue
293              1             1.235                   0.244            blue
294              1             1.249                   0.268            blue
295              2             2.251                   0.141            blue
296              1             3.222                   0.309            blue
297              1             1.292                   0.171            blue
298              1             1.674                   0.304            blue
299              3             0.409                   0.316            blue
300              3             0.713                   0.152            blue
301              3             1.186                   0.172            blue
302              2              0.68                   0.186            blue
303              3             2.819                   0.222            blue
304              2             0.856                   0.208            blue
305              2              1.33                   0.254            blue
306              1             0.528                   0.153            blue
307              3             1.179                    0.31            blue
308              2             0.439                   0.156            blue
309              2             1.025                   0.197            blue
310              3             1.456                   0.235            blue
311              3             0.923                   0.229            blue
312              1              0.79                   0.168            blue
313              2             1.676                   0.291            blue
314              2             0.907                   0.272     purple-blue
315              1              0.98                    0.19     purple-blue
316              2             1.075                   0.222     purple-blue
317              3             0.853                   0.303     purple-blue
318              3             0.786                   0.209     purple-blue
319              2             0.811                   0.209     purple-blue
320              2             0.781                   0.357     purple-blue
321              1             0.945                   0.208     purple-blue
322              1             0.889                   0.156     purple-blue
323              3             0.747                   0.303     purple-blue
324              3              1.15                   0.395     purple-blue
325              1             0.972                   0.312     purple-blue
326              3              1.03                   0.188     purple-blue
327              2             0.835                   0.283     purple-blue
328              1             0.832                   0.303     purple-blue
329              1             0.974                   0.371     purple-blue
330              3             1.052                   0.342     purple-blue
331              2              0.92                   0.287     purple-blue
332              3              0.72                   0.182     purple-blue
333              2             0.927                   0.342     purple-blue
334              1             0.832                    0.19     purple-blue
335              1             0.637                   0.158          yellow
336              3              0.64                   0.258          yellow
337              3             0.771                    0.16          yellow
338              2             0.726                   0.148          yellow
339              2             0.579                   0.281          yellow
340              1             0.828                   0.214          yellow
341              1             0.648                   0.233          yellow
342              1             1.045                    0.24            blue
343              1             1.112                   0.368            blue
344              2             1.168                   0.318            blue
345              3             1.259                   0.376            blue
346              2             0.916                   0.221            blue
347              3             0.898                   0.216            blue
348              1             0.521                   0.267       pale blue
349              2             0.762                   0.265       pale blue
350              1             1.053                   0.256       pale blue
351              2             1.117                   0.263       pale blue
352              3             1.045                   0.213       pale blue
353              3             0.808                   0.321       pale blue
354              1             0.919                    0.26       pale blue
355              2             1.732                   0.638             red
356              3              1.63                   0.586             red
357              3              1.47                   0.517             red
358              1             1.957                   0.478             red
359              2             1.462                   0.489             red
360              1             1.906                   0.461             red
361              3             1.667                   0.799             red
362              1             1.281                   0.661             red
363              2             1.985                    0.44             red
364              2             1.419                   0.226          purple
365              2             1.031                    0.23          purple
366              1               1.4                   0.251          purple
367              1             1.198                   0.292          purple
368              3             1.219                    0.18          purple
369              3             0.823                   0.392          purple
370              1             1.004                   0.255          purple
371              2             1.127                   0.354          purple
372              3             1.127                    0.31          purple
373              3             0.796                    0.16          purple
374              3             0.566                   0.193          purple
375              2             0.679                   0.155          purple
376              1             0.804                   0.375          purple
377              2             0.795                   0.321          purple
378              3             0.574                   0.287          purple
379              3             0.703                   0.282          purple
380              1             0.763                   0.293          purple
381              2             0.889                   0.231          purple
382              2             0.728                   0.314          purple
383              1             0.908                    0.26          purple
384              1             0.941                   0.298          purple
385              3             0.715                   0.129          purple
386              1             0.533                   0.139          purple
387              2              0.57                   0.127          purple
388              1             0.605                   0.182          purple
389              2             0.517                   0.109          purple
390              3             0.818                   0.134          purple
391              1             1.857                   0.469            pink
392              3             1.523                   0.455            pink
393              3             1.854                   0.296            pink
394              2             1.901                   0.493            pink
395              1             1.802                   0.261            pink
396              2             1.508                   0.235            pink
397              2             0.767                   0.207            blue
398              3             0.705                   0.182            blue
399              1             0.654                   0.281            blue
400              1             0.911                   0.242            blue
401              3             0.793                   0.256            blue
402              1              0.66                   0.322            blue
403              1             0.478                   0.149            blue
404              2             0.802                    0.12            blue
405              2             0.643                   0.207            blue
406              1             0.771                   0.111            blue
407              1             0.734                   0.223            blue
408              2             0.582                   0.137            blue
409              3             0.682                   0.284            blue
410              3             0.553                   0.168            blue
411              2             0.668                    0.28            blue
412              3             0.433                   0.168            blue
413              3             0.479                   0.109            blue
414              2              0.47                   0.172            blue
415              3             0.885                   0.089            blue
416              2              0.54                   0.134            blue
417              2             0.639                    0.16            blue
418              1              0.52                   0.146            blue
419              1             0.625                   0.117            blue
420              3             0.479                   0.131            blue
421              3             0.831                   0.201            blue
422              3             0.752                   0.205            blue
423              1             0.852                   0.195            blue
424              2             0.686                   0.286            blue
425              1             0.615                   0.168            blue
426              2              0.78                   0.151            blue
427              3             0.933                   0.258           white
428              2             0.947                   0.288           white
429              1             0.536                   0.191           white
430              1             0.737                   0.219           white
431              2             0.524                   0.222           white
432              3             0.697                    0.25           white
433              1              0.84                   0.232     pale purple
434              3             0.721                   0.229     pale purple
435              1             0.716                   0.132     pale purple
436              2             0.823                   0.222     pale purple
437              2             0.769                   0.179     pale purple
438              3             0.794                   0.206     pale purple
439              3             0.576                   0.215     pale purple
440              1             0.798                   0.279     pale purple
441              2             0.694                   0.173     pale purple
442              3             1.148                   0.315          purple
443              2             1.048                   0.383          purple
444              1             1.025                   0.264          purple
445              3              1.05                   0.251          purple
446              1             1.018                   0.357          purple
447              2             1.183                   0.239          purple
448              3             0.626                   0.153          purple
449              1              0.72                   0.248          purple
450              2             0.552                    0.11          purple
451              1             0.359                   0.202          purple
452              3             0.765                   0.196          purple
453              2             0.703                    0.22          purple
454              3             0.791                   0.277          purple
455              1             0.756                   0.231          purple
456              1             0.819                   0.165          purple
457              2             0.801                   0.255          purple
458              1             0.993                   0.264          purple
459              3             0.735                    0.16          purple
460              2             0.861                   0.217          purple
461              3             0.814                   0.238          purple
462              2             0.864                   0.195          purple
463              1             0.613                   0.127          purple
464              3             0.723                   0.251          purple
465              1             0.713                   0.182          purple
466              2             0.627                   0.169          purple
467              3             0.584                   0.149          purple
468              2             0.752                   0.183          purple
469              1             0.631                    0.16          purple
470              2             0.582                   0.137          purple
471              3              0.71                   0.151          purple
472              2             0.613                   0.137          purple
473              1             0.697                   0.193          purple
474              3             0.667                   0.179          purple
475              1             1.147                   0.182            pink
476              3             0.603                   0.216            pink
477              2             0.954                   0.123            pink
478              1             0.837                   0.335            pink
479              3             0.658                    0.09            pink
480              2             0.913                   0.192            pink
481              2             1.147                   0.353     blue-purple
482              1             1.333                   0.372     blue-purple
483              1             1.158                   0.276     blue-purple
484              3             1.407                   0.414     blue-purple
485              2             1.125                   0.313     blue-purple
486              3             1.264                   0.359     blue-purple
487              3             1.102                   0.291     blue-purple
488              2              1.48                    0.38     blue-purple
489              1             1.345                   0.289     blue-purple
490              2             1.175                   0.385            pink
491              1             1.241                   0.432            pink
492              3             1.081                   0.378            pink
493              2             0.653                    0.13          purple
494              1             0.849                   0.172          purple
495              3             0.611                   0.149     blue-violet
496              1             0.755                   0.188     blue-violet
497              2             0.733                   0.147     blue-violet
498              1             0.902                    0.11    bluish-white
499              3             0.775                   0.137    bluish-white
500              2             0.775                   0.133    bluish-white
501              1             0.881                   0.201          purple
502              3             0.895                   0.184          purple
503              2             0.888                   0.173          purple
504              3             1.046                   0.184          purple
505              1             0.955                   0.238          purple
506              2             1.153                   0.191          purple
507              3             0.877                   0.239          purple
508              2             0.816                   0.203          purple
509              1              0.98                   0.202          purple
510              1             1.079                   0.202          purple
511              1             1.301                   0.296            pink
512              3             1.055                   0.256            pink
513              3             0.842                   0.179            pink
514              2             0.819                   0.249            pink
515              1             1.872                   0.322            pink
516              1              0.96                   0.347            pink
517              3              0.71                   0.242            pink
518              2              1.22                   0.386            pink
519              2             1.115                   0.297            pink
520              2             1.022                   0.278            pink
521              1             0.918                   0.193            pink
522              1             0.805                    0.16            pink
523              3             1.324                   0.281            pink
524              2             0.854                    0.14            pink
525              3              1.33                   0.286            pink
526              2             1.021                   0.289            pink
527              3             0.658                    0.22            pink
528              2             0.632                   0.195            pink
529              1                 1                   0.224            pink
530              1             0.807                   0.266            pink
531              1             0.704                   0.179            pink
532              1             1.093                   0.371            pink
533              2             0.897                   0.229            pink
534              2             0.755                   0.185            pink
535              3             0.738                   0.252            pink
536              3             0.717                   0.184            pink
537              3             1.016                   0.301            pink
538              2             0.621                   0.103            pink
539              1             0.627                    0.13            pink
540              1             2.618                    0.41            pink
541              2             1.965                   0.252            pink
542              3             0.714                   0.151            pink
543              3             0.655                   0.109            pink
544              2             1.611                   0.383            pink
545              1             1.492                    0.33            pink
546              3              1.42                    0.35            pink
547              1             1.634                   0.405            pink
548              3             1.254                   0.282            pink
549              3             1.757                   0.218            pink
550              3              2.05                   0.381            pink
551              1             1.466                   0.391            pink
552              2             1.584                   0.244            pink
553              1             0.505                   0.116            pink
554              2             0.537                   0.099            pink
555              2              1.42                    0.32            pink
556              1             0.567                   0.224            pink
557              2             0.718                   0.195            pink
558              3             0.596                   0.171            pink
559              1             0.643                   0.193          purple
560              1             0.964                   0.348          purple
561              2             1.162                   0.365          purple
562              3             1.171                   0.411          purple
563              1              1.17                    0.44          purple
564              1             1.453                   0.378          purple
565              2             1.128                   0.347          purple
566              2             1.068                   0.298          purple
567              3             0.924                   0.356          purple
568              3             1.248                   0.384          purple
569              2             1.226                   0.355          purple
570              2             0.915                   0.366          purple
571              3              0.75                   0.232          purple
572              1             1.074                   0.351          purple
573              3             1.279                   0.366          purple
574              3             0.629                   0.197            blue
575              2             0.561                   0.219            blue
576              3              0.38                    0.19            blue
577              1             0.847                   0.166            blue
578              1             0.434                   0.188            blue
579              2             0.504                   0.166            blue
580              3             0.551                   0.196            blue
581              2             0.712                    0.19            blue
582              1             0.395                   0.168            blue
583              3             0.557                   0.092          yellow
584              2             0.519                    0.07          yellow
585              1             0.888                   0.204          yellow
586              2             0.796                   0.211          yellow
587              1             0.505                   0.073          yellow
588              2             1.363                   0.596     blue-purple
589              1             1.237                   0.554     blue-purple
590              2             1.455                   0.664     blue-purple
591              1             1.472                   0.683     blue-purple
592              3             1.376                   0.517     blue-purple
593              2             1.603                    0.63     blue-purple
594              1             1.635                    0.52     blue-purple
595              3             1.356                   0.647     blue-purple
596              3             0.802                   0.246          purple
597              2             0.647                   0.198          purple
598              2             0.363                   0.252          purple
599              1             0.606                   0.256          purple
600              3             0.532                   0.295          purple
601              1             0.649                   0.237          purple
602              3             0.511                   0.204          purple
603              1             0.813                   0.382          purple
604              2             0.613                    0.27          purple
605              3             0.638                   0.287          purple
606              1             0.297                   0.272          purple
607              3             0.378                   0.227          purple
608              2             0.436                   0.287          purple
609              2             0.435                   0.182          purple
610              1             0.503                   0.206          purple
611              1             1.238                    0.22           white
612              2             0.997                   0.192           white
613              3             0.961                   0.179           white
614              3             0.551                   0.101           white
615              1             0.464                   0.107           white
616              2             0.457                    0.11           white
617              1             0.523                   0.159           white
618              2             0.447                   0.116           white
619              3             0.317                   0.103           white
620              3             2.485                    0.39             red
621              1             2.233                   0.493             red
622              3             2.332                   0.339             red
623              1             2.093                   0.386             red
624              3              2.32                   0.394             red
625              2             1.706                   0.319             red
626              1             2.297                   0.209             red
627              3             2.361                   0.322             red
628              2             2.658                   0.419             red
629              1             4.108                   0.301             red
630              1             2.075                   0.267             red
631              2             2.483                   0.331             red
632              2             2.226                   0.486             red
633              3             3.381                   0.277             red
634              3             2.212                   0.246             red
635              2             2.603                   0.274             red
636              1             2.073                   0.383             red
637              2             2.716                    0.32             red
638              3             2.749                   0.247             red
639              1             2.738                   0.427             red
640              2             2.848                   0.508             red
641              3             0.342                   0.046           white
642              1             0.449                   0.074           white
643              2             0.485                   0.069           white
644              2             0.524                   0.135           white
645              1             0.535                   0.055           white
646              3             0.327                   0.045           white
647              2             0.432                   0.078           white
648              1             0.685                   0.192           white
649              3              0.78                   0.162           white
650              2             0.476                   0.159      light pink
651              1              0.61                   0.121      light pink
652              3             0.429                   0.087      light pink
653              1             0.592                   0.102      light pink
654              3              0.53                    0.11      light pink
655              2             0.604                   0.132      light pink
656              1             0.412                    0.05      light pink
657              2             0.347                   0.076      light pink
658              3              0.33                   0.104      light pink
659              2             1.963                   0.438      light pink
660              1             1.784                    0.19      light pink
661              3             1.844                   0.287      light pink
662              2             1.562                   0.162      light pink
663              3             1.893                   0.209      light pink
664              3             1.483                   0.212      light pink
665              1             1.702                   0.275      light pink
666              1             1.453                   0.243      light pink
667              2             1.854                   0.203      light pink
668              3             3.866                   0.226    light purple
669              1             3.717                    0.25    light purple
670              2              4.34                   0.252    light purple
671              1             5.099                   0.365    light purple
672              2             5.484                   0.368    light purple
673              3             5.271                   0.299    light purple
674              3             4.335                   0.243    light purple
675              3              4.99                   0.259    light purple
676              3             2.837                   0.203    light purple
677              2             3.882                   0.361    light purple
678              3             4.157                   0.339    light purple
679              2             2.507                   0.216    light purple
680              1             5.339                   0.319    light purple
681              2             4.864                   0.273    light purple
682              1             4.693                   0.222    light purple
683              1             3.872                   0.261    light purple
684              1             4.339                   0.283    light purple
685              2              4.86                   0.256    light purple
686              1             4.344                     0.3    light purple
687              3             3.756                   0.267    light purple
688              2             4.744                   0.281    light purple
689              2             1.002                   0.261          purple
690              2             1.857                   0.203          purple
691              3             2.139                   0.212          purple
692              3             0.961                   0.173          purple
693              1             1.568                   0.252          purple
694              3             3.688                   0.202          purple
695              2             1.102                   0.275          purple
696              1             2.945                   0.278          purple
697              1             2.253                   0.227          purple
698              2              1.05                   0.157          purple
699              2              2.58                   0.235          purple
700              3             1.507                   0.171          purple
701              2             2.512                    0.26          purple
702              1             1.301                     0.2          purple
703              1             1.022                   0.265          purple
704              3             1.037                   0.304          purple
705              1             2.876                   0.231          purple
706              3             2.872                   0.226          purple
707              2             0.902                   0.215          purple
708              1             0.845                   0.175          purple
709              3             0.911                   0.193          purple
710              3             1.311                   0.193          purple
711              1             0.872                   0.203          purple
712              3             1.051                   0.209          purple
713              1             1.192                   0.213          purple
714              1             1.174                   0.216          purple
715              2             0.744                   0.151          purple
716              2             1.303                   0.241          purple
717              2             1.164                   0.212          purple
718              3             1.063                   0.264          purple
719              3              0.89                   0.207          purple
720              1             1.275                   0.233          purple
721              2             1.087                   0.239          purple
722              3             0.773                   0.227          purple
723              1             0.915                    0.19          purple
724              3             1.023                    0.28          purple
725              2             1.008                   0.236          purple
726              2             0.522                   0.151          purple
727              1             0.902                   0.279          purple
728              3             0.688                   0.231          purple
729              1             1.257                   0.279          purple
730              2             0.789                   0.207          purple
731              2             0.882                   0.227          purple
732              1             1.023                   0.289          purple
733              3             0.624                   0.159          purple
734              1             1.049                   0.248          purple
735              1             0.907                   0.285          purple
736              1             0.539                   0.078          purple
737              2             0.766                   0.263          purple
738              2             1.211                   0.279          purple
739              1              1.18                    0.22          purple
740              3             0.928                   0.225          purple
741              3             0.896                   0.248          purple
742              3             0.921                   0.264          purple
743              2             0.912                   0.239          purple
744              2              0.99                   0.259          purple
745              3             1.052                   0.269          purple
746              1             0.585                   0.175           white
747              3             0.279                   0.128           white
748              1             0.349                   0.081           white
749              2             0.195                   0.101           white
750              3             0.392                   0.137           white
751              2             0.292                   0.085           white
752              1             0.397                   0.135           white
753              2             0.439                   0.085           white
754              1             1.029                   0.167          purple
755              1             0.712                   0.137          purple
756              1             0.934                    0.17          purple
757              2             0.854                    0.14          purple
758              3             0.948                   0.133          purple
759              1             1.047                   0.149          purple
760              2             1.045                   0.156          purple
761              3             0.884                   0.114          purple
762              2              0.98                    0.15          purple
763              3             1.002                   0.165          purple
764              3             0.975                    0.13          purple
765              1             0.892                     0.1          purple
766              3             0.697                    0.14          purple
767              3             1.049                   0.137          purple
768              2             0.933                   0.134          purple
769              2             0.887                   0.131          purple
770              1             1.097                   0.194          purple
771              1             1.407                   0.123          purple
772              2             0.594                   0.151          purple
773              2              0.89                   0.213          purple
774              3              0.56                   0.202          purple
775              2              0.89                   0.144           white
776              3             0.716                   0.113           white
777              1             0.995                   0.129           white
778              3             0.471                   0.182           white
779              2             1.117                   0.192           white
780              2             0.482                   0.129           white
781              1             0.713                    0.11           white
782              3             1.166                   0.173           white
783              2             1.159                   0.188           white
784              1             1.061                   0.168           white
785              3             0.878                   0.119           white
786              3             0.905                   0.115           white
787              2              0.69                   0.099           white
788              1              1.15                   0.168           white
789              2              0.87                    0.16           white
790              1             0.835                   0.164           white
791              1             0.609                   0.124           white
792              3             0.869                   0.258           white
793              2             2.998                   0.491             red
794              1             3.234                   0.522             red
795              3             2.935                    0.51             red
796              3             3.216                   0.432             red
797              2             2.683                    0.41             red
798              1              2.71                   0.459             red
799              1             0.841                   0.155             red
800              2             1.101                   0.156             red
801              2             3.687                   0.225           white
802              1             4.215                   0.274           white
803              1             3.587                   0.291           white
804              3             4.477                   0.315           white
805              2             5.085                    0.34           white
806              3             3.334                   0.275           white
807              2              4.45                   0.263          purple
808              3             3.486                   0.253          purple
809              2             3.544                   0.364          purple
810              1             3.063                   0.368          purple
811              1             2.731                   0.324          purple
812              3             3.687                   0.272          purple
813              2             2.604                   0.333          purple
814              1             3.584                   0.332          purple
815              2             2.197                   0.339          purple
816              3             2.601                   0.304          purple
817              1             4.359                   0.326          purple
818              2             3.069                   0.366          purple
819              3             2.194                   0.314          purple
820              1             2.241                   0.293          purple
821              3             4.408                   0.315          purple
822              2              1.82                   0.385           white
823              2             1.249                   0.196           white
824              1             1.403                   0.391           white
825              3             1.065                   0.438           white
826              3             1.071                   0.303           white
827              2              1.46                   0.401           white
828              3             1.418                   0.316           white
829              1             0.879                   0.279           white
830              1             1.928                   0.428           white
831              3             1.026                   0.188           white
832              3             1.011                   0.306           white
833              2             1.213                   0.293           white
834              2             0.806                   0.234           white
835              1             1.018                   0.183           white
836              2              1.07                   0.334           white
837              1              0.79                   0.306           white
838              3             1.219                   0.231           white
839              1             0.967                   0.293           white
840              3             0.942                   0.288           white
841              2              1.13                   0.324           white
842              1             1.013                   0.235           white
843              1             0.977                   0.178           white
844              2             2.644                    0.21           white
845              1             2.223                   0.418           white
846              1             2.863                   0.289           white
847              3             2.616                   0.271           white
848              2             2.602                   0.248           white
849              1             1.855                   0.148           white
850              2             2.158                   0.464           white
851              3             2.261                    0.42           white
852              3             2.084                    0.19           white
853              1             2.102                   0.221            pink
854              1             1.992                    0.18            pink
855              3             1.955                   0.161            pink
856              3             0.886                   0.205            pink
857              3             1.764                   0.163            pink
858              1             1.765                   0.254            pink
859              2             1.601                   0.281            pink
860              2             0.953                   0.141            pink
861              1             1.035                   0.141            pink
862              2             1.407                   0.184            pink
863              2             1.779                   0.186            pink
864              3             1.378                   0.147            pink
865              3               2.3                   0.353           white
866              2             2.007                   0.383           white
867              1             2.102                   0.246           white
868              2             1.145                    0.27           white
869              1              1.36                   0.288           white
870              3             1.259                   0.246           white
871              2             2.994                   0.353          purple
872              1             2.765                   0.326          purple
873              2             2.264                   0.341          purple
874              3             2.482                    0.32          purple
875              1             2.142                   0.392          purple
876              3             2.551                   0.464          purple
877              1               0.4                   0.181           white
878              1             0.459                   0.188           white
879              2              0.55                   0.131           white
880              2             0.433                   0.165           white
881              3             0.451                     0.2           white
882              3             0.451                   0.168           white
883              3               0.6                   0.172           white
884              2             0.515                   0.175           white
885              1             0.561                   0.199           white
886              2               1.4                   0.247           white
887              1             1.591                    0.29           white
888              1             1.331                   0.283           white
889              3             1.096                   0.253           white
890              3             1.296                   0.254           white
891              2              1.31                   0.243           white
892              2              1.12                   0.212           white
893              1             1.244                    0.34           white
894              3             1.566                   0.241           white
895              2             1.444                     0.2           white
896              3             1.467                   0.288           white
897              1             1.356                   0.246           white
898              2             3.088                    0.23           white
899              2             1.574                   0.216           white
900              1              1.81                   0.225           white
901              1              2.93                   0.183           white
902              3             1.087                   0.191           white
903              3             2.834                   0.302           white
904              1             0.348                   0.186           white
905              1             0.476                   0.178           white
906              3             0.359                    0.11           white
907              3             0.437                   0.184           white
908              2             0.422                   0.134           white
909              2             0.387                   0.191           white
910              1             0.394                   0.146           white
911              1             0.546                   0.112           white
912              3             0.369                    0.11           white
913              2             0.496                   0.207           white
914              2             0.421                   0.208           white
915              3             0.416                   0.133           white
916              1             2.941                   0.465            pink
917              2             2.624                   0.481            pink
918              1             2.736                   0.465            pink
919              2             2.141                   0.291            pink
920              2             2.427                    0.37            pink
921              1              2.64                   0.283            pink
922              3              2.15                    0.25            pink
923              1             2.937                   0.391            pink
924              3             3.217                   0.351            pink
925              3             3.068                   0.311            pink
926              3             2.186                   0.287            pink
927              2             2.265                   0.256            pink
928              1             2.781                    0.39            pink
929              3             2.596                   0.304            pink
930              3             1.851                   0.341            pink
931              2             3.576                   0.341            pink
932              1             3.364                   0.314            pink
933              2             3.151                   0.378            pink
934              1             1.976                   0.483            pink
935              3             1.867                   0.355            pink
936              1             1.582                    0.42            pink
937              2             1.553                   0.376            pink
938              3              1.68                   0.371            pink
939              1             1.999                   0.542            pink
940              2             1.877                   0.398            pink
941              2             1.974                   0.434            pink
942              3             1.615                   0.394            pink
943              1             2.471                   0.307           white
944              3              2.06                   0.276           white
945              3             2.443                   0.324           white
946              3             2.579                   0.617           white
947              2             2.885                   0.541           white
948              1             2.423                   0.237           white
949              3             2.293                   0.431           white
950              2             2.879                   0.343           white
951              2             2.172                   0.359           white
952              1             2.139                   0.384           white
953              2             2.745                   0.397           white
954              1             2.284                    0.27           white
955              1             1.355                   0.357     blue-purple
956              3             1.589                   0.855     blue-purple
957              1             1.812                   0.568     blue-purple
958              2             1.659                   0.594     blue-purple
959              2              1.36                   0.364     blue-purple
960              3              1.05                   0.351     blue-purple
961              2             1.613                   0.277             red
962              3             1.744                   0.396             red
963              1             1.946                   0.267             red
964              1             0.592                   0.118           white
965              3              0.47                   0.138           white
966              1               0.6                   0.204           white
967              3              0.62                   0.099           white
968              2             0.417                   0.134           white
969              2             0.607                   0.096           white
970              2             0.875                   0.204            pink
971              1             0.933                   0.136            pink
972              3             1.112                   0.161            pink
973              2             0.884                   0.205            pink
974              3             1.455                   0.211            pink
975              3             0.905                   0.148            pink
976              1             1.322                   0.244            pink
977              2              1.19                   0.156            pink
978              3             0.834                   0.117            pink
979              2             0.803                   0.137            pink
980              2             0.654                   0.126            pink
981              1             0.679                     0.2            pink
982              1             0.831                   0.135            pink
983              1             0.884                   0.147            pink
984              3             0.873                   0.133            pink
985              3             0.584                   0.106      light blue
986              2             0.501                   0.108      light blue
987              1             0.464                    0.11      light blue
988              1             0.522                   0.099            pink
989              2             0.457                   0.127            pink
990              3             0.474                   0.079            pink
991              2             0.829                   0.129            pink
992              2             0.459                   0.096            pink
993              3             0.833                   0.132            pink
994              1             0.551                   0.126            pink
995              1             0.675                   0.099            pink
996              1             0.272                    0.05            pink
997              3             0.358                   0.073            pink
998              2             0.361                   0.093            pink
999              3             0.339                   0.118            pink
1000             2             0.532                   0.109            pink
1001             1             0.547                   0.116            pink
1002             1              0.51                   0.053          purple
1003             2             0.449                    0.06          purple
1004             3             0.486                   0.067          purple
1005             2             0.707                   0.133           white
1006             3             0.695                   0.108           white
1007             1             0.862                   0.132           white
1008             3             0.581                    0.11           white
1009             2             0.652                   0.119           white
1010             1             0.784                   0.101           white
1011             1             0.461                   0.112          purple
1012             2             0.351                   0.136          purple
1013             3             0.996                   0.183            pink
1014             1             1.142                   0.254            pink
1015             1              1.27                   0.215            pink
1016             2              1.14                   0.166            pink
1017             2             0.858                   0.119            pink
1018             3             1.345                   0.263            pink
1019             2             0.833                   0.214            pink
1020             1             1.585                   0.233            pink
1021             3             1.068                   0.193            pink
1022             2             1.411                   0.259            pink
1023             3             1.287                   0.211            pink
1024             1             0.951                   0.156            pink
1025             2             0.543                   0.109           white
1026             3             0.464                   0.116           white
1027             1             0.716                   0.151           white
1028             1             0.601                   0.137          purple
1029             3             0.697                   0.182          purple
1030             2              0.58                   0.152          purple
1031             2             0.422                   0.129          purple
1032             1             0.539                   0.126          purple
1033             3             0.499                   0.145          purple
1034             3             0.629                    0.17            blue
1035             2             0.698                   0.067            blue
1036             3             0.603                   0.092            blue
1037             2             0.662                    0.13            blue
1038             3             0.547                   0.142            blue
1039             1              0.71                   0.107            blue
1040             2             0.758                   0.115            blue
1041             1             0.761                   0.148            blue
1042             1             0.927                   0.165            blue
1043             2             0.763                   0.128            blue
1044             1             0.682                   0.112            blue
1045             2              0.57                   0.148           white
1046             2             0.692                   0.168           white
1047             1             0.968                   0.127           white
1048             1             0.596                   0.122           white
1049             2             0.949                   0.179          purple
1050             3             1.001                   0.196          purple
1051             1              1.05                   0.152          purple
1052             3             0.805                    0.21          purple
1053             1             0.962                   0.266          purple
1054             2             0.737                   0.237          purple
1055             2             1.898                   0.264           white
1056             3             1.753                   0.224           white
1057             2             1.661                   0.192           white
1058             1             1.802                   0.176           white
1059             3             1.982                   0.226           white
1060             1             1.981                   0.215           white
1061             2             2.084                   0.209           white
1062             3             1.916                   0.223           white
1063             1             2.393                   0.241           white
1064             3             1.336                   0.138          purple
1065             2             1.756                   0.174          purple
1066             2             1.215                   0.151          purple
1067             3             2.083                   0.173          purple
1068             1             1.539                   0.191          purple
1069             1             1.171                   0.147          purple
1070             1             2.203                   0.194          purple
1071             1             1.634                   0.179          purple
1072             1             2.166                   0.119          purple
1073             2             2.072                   0.157          purple
1074             3             1.345                   0.179          purple
1075             3             1.534                   0.222          purple
1076             1              1.88                   0.236          purple
1077             2             1.853                   0.187          purple
1078             2             1.742                   0.191          purple
1079             3             1.554                   0.199          purple
1080             3             1.482                   0.125          purple
1081             3             2.233                   0.172          purple
1082             2             1.595                   0.165          purple
1083             2             1.382                   0.168          purple
1084             1             1.463                   0.124          purple
1085             3             2.395                   0.212          purple
1086             2             1.496                   0.243          purple
1087             1             1.372                   0.228          purple
1088             1              2.29                   0.293          purple
1089             2             1.666                   0.291          purple
1090             2             1.664                   0.224          purple
1091             3             1.835                    0.24          purple
1092             1             1.953                   0.227          purple
1093             3              1.45                   0.242          purple
1094             1             1.916                    0.18          purple
1095             2             2.355                   0.175          purple
1096             3             1.596                   0.203          purple
1097             2             2.051                   0.246          purple
1098             3             2.273                   0.166          purple
1099             2             2.161                   0.191          purple
1100             3             1.989                   0.181          purple
1101             1             2.172                   0.207          purple
1102             1             3.002                   0.286          purple
1103             2             3.175                    0.23          purple
1104             3             2.526                   0.207          purple
1105             1             2.704                   0.161          purple
1106             1             1.944                   0.173          purple
1107             1              1.92                   0.214          purple
1108             2             2.386                   0.238          purple
1109             2             1.904                   0.224          purple
1110             3             2.338                   0.235          purple
1111             3             2.398                   0.198          purple
1112             2             2.411                   0.248          purple
1113             1             2.192                   0.207          purple
1114             3             2.144                    0.19          purple
1115             2              2.05                   0.141          purple
1116             1             2.345                   0.244          purple
1117             1             3.488                   0.153          purple
1118             2             4.056                   0.244          purple
1119             3              5.85                   0.456          purple
1120             1             4.589                   0.266          purple
1121             3             3.312                   0.288          purple
1122             2             4.551                   0.258          purple
1123             3             2.735                   0.278          purple
1124             1             2.806                    0.25          purple
1125             1             3.483                   0.289          purple
1126             2             3.014                   0.311          purple
1127             1             3.334                   0.251          purple
1128             2             3.605                   0.253          purple
1129             3             2.341                   0.227          purple
1130             2             2.521                   0.223          purple
1131             2              2.55                   0.172          purple
1132             3             3.027                   0.295          purple
1133             3             2.649                   0.205          purple
1134             3             2.377                   0.278          purple
1135             2             3.009                   0.241          purple
1136             1             2.523                   0.209          purple
1137             1             3.179                   0.311          purple
1138             1             2.906                   0.261          purple
1139             3              2.58                   0.249          purple
1140             2             2.863                   0.273          purple
1141             2             2.647                   0.341          purple
1142             1              3.15                    0.36          purple
1143             1             2.555                   0.267          purple
1144             3             2.595                   0.268          purple
1145             2             2.202                   0.204          purple
1146             3             2.209                    0.28          purple
1147             3             2.014                   0.218          purple
1148             3              3.05                   0.305          purple
1149             2             2.275                    0.21          purple
1150             2             2.562                   0.275          purple
1151             1             2.697                   0.269          purple
1152             1              2.66                   0.246          purple
1153             1             1.553                    0.35          purple
1154             3             1.892                   0.211          purple
1155             2             1.634                    0.28          purple
1156             2             1.716                   0.472          purple
1157             1             1.972                   0.284          purple
1158             2             1.706                   0.276          purple
1159             3             2.584                    0.29          purple
1160             1             2.092                   0.285          purple
1161             3             2.081                   0.188          purple
1162             2              2.88                   0.261          purple
1163             1              2.84                   0.199          purple
1164             2             2.335                   0.251          purple
1165             2             3.034                   0.233          purple
1166             3             3.023                   0.225          purple
1167             1             3.184                   0.234          purple
1168             3             3.105                   0.275          purple
1169             1             2.561                   0.237          purple
1170             3             3.001                    0.31          purple
1171             1             2.579                   0.307          purple
1172             2              3.08                   0.316          purple
1173             3             3.103                   0.326          purple
1174             1             3.541                   0.278          purple
1175             2             3.808                   0.296          purple
1176             1             3.965                   0.385          purple
1177             2             3.794                   0.324          purple
1178             3             3.351                   0.378          purple
1179             2             2.511                   0.287          purple
1180             3             3.128                   0.238          purple
1181             1             1.655                   0.232          purple
1182             2             1.411                    0.26          purple
1183             1             2.452                   0.308          purple
1184             2             3.021                     0.3          purple
1185             3             3.253                   0.211          purple
1186             3             1.732                   0.264          purple
1187             1             3.035                   0.224          purple
1188             3              1.96                   0.299          purple
1189             1             1.643                   0.242          purple
1190             2             2.366                   0.232          purple
1191             1             2.241                   0.248          purple
1192             2             1.848                   0.233          purple
1193             1             2.282                   0.339          purple
1194             2             1.616                   0.201          purple
1195             3             1.899                   0.279          purple
1196             1             2.207                   0.193          purple
1197             2             2.352                   0.155          purple
1198             3             2.146                   0.219          purple
1199             3             2.085                   0.203          purple
1200             1             2.078                   0.185          purple
1201             2             2.394                   0.172          purple
1202             3             1.984                   0.181          purple
1203             1             2.139                   0.162          purple
1204             2              2.44                   0.239          purple
1205             1             2.596                   0.283          purple
1206             3             2.512                   0.235          purple
1207             2              2.61                   0.228          purple
1208             1             1.945                   0.188          purple
1209             2              1.85                   0.217          purple
1210             3              1.58                   0.206          purple
1211             1              3.36                   0.256          purple
1212             2             2.821                   0.211          purple
1213             1             2.781                   0.204          purple
1214             2               3.2                   0.204          purple
1215             3             3.165                   0.308          purple
1216             1             3.369                    0.25          purple
1217             2             3.036                   0.271          purple
1218             2             3.871                   0.302          purple
1219             3             3.094                   0.283          purple
1220             1             2.407                   0.238          purple
1221             3             2.622                   0.234          purple
1222             1             3.879                   0.294          purple
1223             1              1.53                   0.162          purple
1224             3             1.749                   0.122          purple
1225             1             2.811                   0.277          purple
1226             2              1.13                   0.159          purple
1227             1             1.386                   0.211          purple
1228             2             1.364                   0.202          purple
1229             2             1.581                   0.167          purple
1230             3               1.2                    0.17          purple
1231             3             2.377                   0.255          purple
1232             3             1.427                   0.174          purple
1233             1             1.841                   0.164          purple
1234             2             1.286                    0.18          purple
1235             2             1.825                   0.212          purple
1236             3             1.575                   0.173          purple
1237             3             1.404                   0.221          purple
1238             1             1.608                   0.174          purple
1239             1             1.762                   0.214          purple
1240             1             1.785                   0.217          purple
1241             2             2.934                    0.23          purple
1242             1              0.78                   0.425            blue
1243             2             0.738                   0.341            blue
1244             3             0.866                   0.284            blue
1245             2             1.638                   0.459            blue
1246             1             1.286                     0.5            blue
1247             1             0.971                   0.281            blue
1248             3             0.802                   0.198            blue
1249             1             1.078                   0.349            blue
1250             2             0.929                   0.528            blue
1251             3             0.964                   0.306            blue
1252             1             1.083                   0.372            blue
1253             2             1.013                   0.344            blue
1254             2             1.002                   0.299            blue
1255             3             0.787                   0.292            blue
1256             1             2.255                   0.577            blue
1257             2             2.636                   0.579            blue
1258             1             3.459                   0.867            blue
1259             2              2.66                   0.524            blue
1260             3             2.104                   0.724            blue
1261             1               1.7                   0.613            blue
1262             2             2.194                   0.826            blue
1263             3             2.014                   0.812            blue
1264             1             2.359                   1.165            blue
1265             1             2.163                   0.849            blue
1266             2             2.043                   0.773            blue
1267             1             2.237                   0.886            blue
1268             2             1.988                   0.819            blue
1269             1             0.845                   0.304          purple
1270             1             0.811                   0.234          purple
1271             1             1.091                   0.437          purple
1272             2              1.18                   0.442          purple
1273             2             0.733                   0.446          purple
1274             2              0.97                   0.439          purple
1275             3             1.192                   0.584          purple
1276             3             1.017                   0.405          purple
1277             3             0.885                   0.266     blue-purple
1278             2             0.811                   0.248     blue-purple
1279             1              0.96                   0.434     blue-purple
1280             1              1.34                   0.384     blue-purple
1281             3             1.095                   0.381     blue-purple
1282             3             0.909                   0.363     blue-purple
1283             2             1.014                   0.322     blue-purple
1284             2             1.237                   0.282     blue-purple
1285             1             0.716                   0.311     blue-purple
1286             1             1.259                   0.262     blue-purple
1287             2             0.891                    0.21     blue-purple
1288             1             1.353                   0.393     blue-purple
1289             1             1.387                   0.472     blue-purple
1290             1             1.007                   0.366     blue-purple
1291             2             1.047                   0.297     blue-purple
1292             3             1.445                   0.483     blue-purple
1293             1             1.045                   0.449     blue-purple
1294             2              1.09                   0.443     blue-purple
1295             3             1.007                   0.247     blue-purple
1296             1             1.419                   0.674     blue-purple
1297             2             1.641                   0.463     blue-purple
1298             2             1.109                   0.295     blue-purple
1299             2              1.18                   0.431     blue-purple
1300             3             1.242                   0.433     blue-purple
1301             1              1.16                    0.39     blue-purple
1302             3             1.209                   0.427     blue-purple
1303             3             1.196                   0.382     blue-purple
1304             1             1.313                   0.359     blue-purple
1305             2             1.265                   0.399     blue-purple
1306             1             1.165                   0.296     blue-purple
1307             2             1.209                    0.36     blue-purple
1308             3             1.279                   0.373     blue-purple
1309             2             1.022                   0.471            blue
1310             2             1.315                   0.504            blue
1311             1             1.154                   0.543            blue
1312             3             0.917                    0.47            blue
1313             3             1.389                   0.435            blue
1314             1             1.023                   0.557            blue
1315             1             1.011                   0.469            blue
1316             2             1.015                   0.478            blue
1317             3             1.004                   0.482            blue
1318             1             0.869                   0.246           white
1319             1             0.572                   0.302           white
1320             1             0.677                   0.165           white
1321             2             0.533                   0.165           white
1322             1             0.621                   0.266           white
1323             2              0.68                    0.17           white
1324             3             0.673                    0.15           white
1325             1             3.036                   0.295          yellow
1326             1             2.594                   0.356          yellow
1327             1             2.285                   0.852            blue
1328             2             1.191                   0.328            blue
1329             3             1.926                   0.869            blue
1330             1             0.861                    0.33            blue
1331             1               1.2                   0.361            blue
1332             2             1.983                   0.922            blue
1333             1             1.197                   0.415            blue
1334             2             1.086                   0.384            blue
1335             1             1.397                   0.501            blue
1336             2             1.582                    0.59            blue
1337             3             1.153                   0.474            blue
1338             1             0.981                    0.39            blue
1339             2             0.877                   0.331            blue
1340             1             1.404                   0.633            blue
1341             3             1.171                   0.483            blue
1342             1             1.275                    0.62            blue
1343             2             1.368                   0.455            blue
1344             2              1.25                   0.436            blue
1345             3             1.037                   0.371            blue
1346             1             1.342                   0.482            blue
1347             3             1.539                   0.361            blue
1348             1             1.517                   0.437            blue
1349             2             1.544                   0.488            blue
1350             2              2.13                   0.486     blue-purple
1351             1             2.304                   0.595     blue-purple
1352             1             1.681                   0.482     blue-purple
1353             2             1.889                   0.729     blue-purple
1354             1             2.071                   0.591     blue-purple
1355             2             1.976                   0.675     blue-purple
1356             3             1.814                   0.718     blue-purple
1357             3             2.057                   0.304     blue-purple
1358             2             1.853                    0.39     blue-purple
1359             1             2.029                   0.317     blue-purple
1360             3             0.891                   0.226           white
1361             3             1.018                   0.344           white
1362             2             0.869                    0.19           white
1363             3             0.688                   0.229           white
1364             1             0.774                   0.255           white
1365             2             0.741                   0.211           white
1366             1             0.757                   0.212           white
1367             2             0.945                   0.279           white
1368             1             0.714                    0.23           white
1369             1             0.857                    0.25           white
1370             2             1.087                   0.397           white
1371             3             0.926                    0.33           white
1372             2             0.718                   0.164           white
1373             3             0.717                   0.238           white
1374             1             0.713                   0.223           white
1375             1              1.15                   0.261          purple
1376             3             0.729                    0.16          purple
1377             2             0.991                    0.25          purple
1378             3             0.943                   0.323          purple
1379             1              1.23                   0.431          purple
1380             2             0.947                   0.211          purple
1381             3             0.965                   0.259          purple
1382             1               0.5                   0.186          purple
1383             2             0.739                   0.145          purple
       color_opt_2 color_opt_3 color_opt_4                     color_source
1             <NA>        <NA>        <NA>                  Hsu,  Hall 2003
2             <NA>        <NA>        <NA>                  Hsu,  Hall 2003
3             <NA>        <NA>        <NA>                  Hsu,  Hall 2003
4             <NA>        <NA>        <NA>                  Hsu,  Hall 2003
5             <NA>        <NA>        <NA>                      Porter 1998
6             <NA>        <NA>        <NA>                      Porter 1998
7             <NA>        <NA>        <NA>                      Porter 1998
8             <NA>        <NA>        <NA>                      Porter 1998
9             <NA>        <NA>        <NA>                      Porter 1998
10            <NA>        <NA>        <NA>                      Porter 1998
11            <NA>        <NA>        <NA>                      Porter 1998
12            <NA>        <NA>        <NA>                      Porter 1998
13            <NA>        <NA>        <NA>                      Porter 1998
14            <NA>        <NA>        <NA>                      Porter 1998
15            <NA>        <NA>        <NA>                      Porter 1998
16            <NA>        <NA>        <NA>                      Porter 1998
17            <NA>        <NA>        <NA>                      Porter 1998
18            <NA>        <NA>        <NA>                      Porter 1998
19            <NA>        <NA>        <NA>                      Porter 1998
20            <NA>        <NA>        <NA>                      Porter 1998
21            <NA>        <NA>        <NA>                      Porter 1998
22            <NA>        <NA>        <NA>                      Porter 1998
23            <NA>        <NA>        <NA>                      Porter 1998
24            <NA>        <NA>        <NA>                      Porter 1998
25            <NA>        <NA>        <NA>                      Porter 1998
26            <NA>        <NA>        <NA>                      Porter 1998
27            <NA>        <NA>        <NA>                      Porter 1998
28            <NA>        <NA>        <NA>                      Porter 1998
29            <NA>        <NA>        <NA>                      Porter 1998
30            <NA>        <NA>        <NA>                      Porter 1998
31            <NA>        <NA>        <NA>                      Porter 1998
32            <NA>        <NA>        <NA>                      Porter 1998
33            <NA>        <NA>        <NA>                      Porter 1998
34            <NA>        <NA>        <NA>                      Porter 1998
35            <NA>        <NA>        <NA>                      Porter 1998
36            <NA>        <NA>        <NA>                      Porter 1998
37            <NA>        <NA>        <NA>                      Porter 1998
38            <NA>        <NA>        <NA>                      Porter 1998
39            <NA>        <NA>        <NA>                      Porter 1998
40            <NA>        <NA>        <NA>                      Porter 1998
41            <NA>        <NA>        <NA>                      Porter 1998
42            <NA>        <NA>        <NA>                      Porter 1998
43            <NA>        <NA>        <NA>                      Porter 1998
44            <NA>        <NA>        <NA>                      Porter 1998
45            <NA>        <NA>        <NA>                      Porter 1998
46            <NA>        <NA>        <NA>                      Porter 1998
47            <NA>        <NA>        <NA>                      Porter 1998
48            <NA>        <NA>        <NA>                      Porter 1998
49            <NA>        <NA>        <NA>                      Porter 1998
50            <NA>        <NA>        <NA>                    Jepson Online
51            <NA>        <NA>        <NA>                    Jepson Online
52            <NA>        <NA>        <NA>                    Jepson Online
53            <NA>        <NA>        <NA>                    Jepson Online
54            <NA>        <NA>        <NA>                    Jepson Online
55            <NA>        <NA>        <NA>                    Jepson Online
56            <NA>        <NA>        <NA>                    Jepson Online
57            <NA>        <NA>        <NA>                    Jepson Online
58            <NA>        <NA>        <NA>                    Jepson Online
59            <NA>        <NA>        <NA>                    Jepson Online
60            <NA>        <NA>        <NA>                    Jepson Online
61            <NA>        <NA>        <NA>                    Jepson Online
62            <NA>        <NA>        <NA>                    Jepson Online
63            <NA>        <NA>        <NA>                    Jepson Online
64            <NA>        <NA>        <NA>                    Jepson Online
65            <NA>        <NA>        <NA>                    Jepson Online
66            <NA>        <NA>        <NA>                    Jepson Online
67            <NA>        <NA>        <NA>                    Jepson Online
68            <NA>        <NA>        <NA>                    Jepson Online
69            <NA>        <NA>        <NA>                    Jepson Online
70            <NA>        <NA>        <NA>                    Jepson Online
71            <NA>        <NA>        <NA>                    Jepson Online
72            <NA>        <NA>        <NA>                    Jepson Online
73            <NA>        <NA>        <NA>                    Jepson Online
74            <NA>        <NA>        <NA>                    Jepson Online
75            <NA>        <NA>        <NA>                    Jepson Online
76            <NA>        <NA>        <NA>                    Jepson Online
77            <NA>        <NA>        <NA>                    Jepson Online
78            <NA>        <NA>        <NA>                    Jepson Online
79            <NA>        <NA>        <NA>                    Jepson Online
80            <NA>        <NA>        <NA>                    Jepson Online
81            <NA>        <NA>        <NA>                    Jepson Online
82            <NA>        <NA>        <NA>                    Jepson Online
83            <NA>        <NA>        <NA>                    Jepson Online
84            <NA>        <NA>        <NA>                    Jepson Online
85            <NA>        <NA>        <NA>                    Jepson Online
86            <NA>        <NA>        <NA>                    Jepson Online
87            <NA>        <NA>        <NA>                    Jepson Online
88            <NA>        <NA>        <NA>                    Jepson Online
89            <NA>        <NA>        <NA>                    Jepson Online
90            <NA>        <NA>        <NA>                    Jepson Online
91            <NA>        <NA>        <NA>                    Jepson Online
92            <NA>        <NA>        <NA>                    Jepson Online
93            <NA>        <NA>        <NA>                    Jepson Online
94            <NA>        <NA>        <NA>                    Jepson Online
95            <NA>        <NA>        <NA>                    Jepson Online
96            <NA>        <NA>        <NA>                    Jepson Online
97            <NA>        <NA>        <NA>                    Jepson Online
98            <NA>        <NA>        <NA>                    Jepson Online
99            <NA>        <NA>        <NA>                    Jepson Online
100           <NA>        <NA>        <NA>                    Jepson Online
101           <NA>        <NA>        <NA>                    Jepson Online
102           <NA>        <NA>        <NA>                    Jepson Online
103           <NA>        <NA>        <NA>                    Jepson Online
104           <NA>        <NA>        <NA>                    Jepson Online
105           <NA>        <NA>        <NA>                    Jepson Online
106           <NA>        <NA>        <NA>                    Jepson Online
107           <NA>        <NA>        <NA>               Grant,  Grant 1965
108           <NA>        <NA>        <NA>               Grant,  Grant 1965
109           <NA>        <NA>        <NA>               Grant,  Grant 1965
110           <NA>        <NA>        <NA>               Grant,  Grant 1965
111           <NA>        <NA>        <NA>               Grant,  Grant 1965
112           <NA>        <NA>        <NA>               Grant,  Grant 1965
113           <NA>        <NA>        <NA>               Grant,  Grant 1965
114           <NA>        <NA>        <NA>               Grant,  Grant 1965
115           <NA>        <NA>        <NA>               Grant,  Grant 1965
116           <NA>        <NA>        <NA>               Grant,  Grant 1965
117           <NA>        <NA>        <NA>               Grant,  Grant 1965
118           <NA>        <NA>        <NA>               Grant,  Grant 1965
119           <NA>        <NA>        <NA>               Grant,  Grant 1965
120           <NA>        <NA>        <NA>               Grant,  Grant 1965
121           <NA>        <NA>        <NA>               Grant,  Grant 1965
122           <NA>        <NA>        <NA>               Grant,  Grant 1965
123           <NA>        <NA>        <NA>               Grant,  Grant 1965
124           <NA>        <NA>        <NA>               Grant,  Grant 1965
125           <NA>        <NA>        <NA>               Grant,  Grant 1965
126           <NA>        <NA>        <NA>               Grant,  Grant 1965
127           <NA>        <NA>        <NA>               Grant,  Grant 1965
128           <NA>        <NA>        <NA>               Grant,  Grant 1965
129           <NA>        <NA>        <NA>               Grant,  Grant 1965
130           <NA>        <NA>        <NA>               Grant,  Grant 1965
131           <NA>        <NA>        <NA>               Grant,  Grant 1965
132           <NA>        <NA>        <NA>               Grant,  Grant 1965
133           <NA>        <NA>        <NA>               Grant,  Grant 1965
134           <NA>        <NA>        <NA>               Grant,  Grant 1965
135           <NA>        <NA>        <NA>               Grant,  Grant 1965
136           <NA>        <NA>        <NA>                    Prather 1999b
137           <NA>        <NA>        <NA>                    Prather 1999b
138           <NA>        <NA>        <NA>                    Prather 1999b
139           <NA>        <NA>        <NA>                    Prather 1999b
140           <NA>        <NA>        <NA>                    Prather 1999b
141           <NA>        <NA>        <NA>                    Prather 1999b
142           <NA>        <NA>        <NA>                    Prather 1999b
143           <NA>        <NA>        <NA>                    Prather 1999b
144           <NA>        <NA>        <NA>                    Prather 1999b
145           <NA>        <NA>        <NA>                    Prather 1999b
146           <NA>        <NA>        <NA>                    Prather 1999b
147           <NA>        <NA>        <NA>                    Prather 1999b
148           <NA>        <NA>        <NA>                    Prather 1999b
149           <NA>        <NA>        <NA>                    Prather 1999b
150           <NA>        <NA>        <NA>                    Prather 1999b
151           <NA>        <NA>        <NA>                    Prather 1999b
152           <NA>        <NA>        <NA>                    Prather 1999b
153           <NA>        <NA>        <NA>                    Prather 1999b
154           <NA>        <NA>        <NA>                    Prather 1999b
155           <NA>        <NA>        <NA>                    Prather 1999b
156           <NA>        <NA>        <NA>                    Prather 1999b
157           <NA>        <NA>        <NA>                    Prather 1999b
158           <NA>        <NA>        <NA>                    Prather 1999b
159           <NA>        <NA>        <NA>                    Prather 1999b
160           <NA>        <NA>        <NA>                    Prather 1999b
161           <NA>        <NA>        <NA>                    Prather 1999b
162           <NA>        <NA>        <NA>                    Prather 1999b
163           <NA>        <NA>        <NA>                    Prather 1999b
164           <NA>        <NA>        <NA>                    Prather 1999b
165           <NA>        <NA>        <NA>                    Milliken 2010
166           <NA>        <NA>        <NA>                    Milliken 2010
167           <NA>        <NA>        <NA>                    Milliken 2010
168           <NA>        <NA>        <NA>                    Milliken 2010
169           <NA>        <NA>        <NA>                    Milliken 2010
170           <NA>        <NA>        <NA>                    Milliken 2010
171           <NA>        <NA>        <NA>                    Milliken 2010
172           <NA>        <NA>        <NA>                    Milliken 2010
173           <NA>        <NA>        <NA>                    Milliken 2010
174           <NA>        <NA>        <NA>                    Milliken 2010
175           <NA>        <NA>        <NA>                    Milliken 2010
176           <NA>        <NA>        <NA>                    Milliken 2010
177           <NA>        <NA>        <NA>                    Milliken 2010
178           <NA>        <NA>        <NA>                    Milliken 2010
179           <NA>        <NA>        <NA>                    Milliken 2010
180           <NA>        <NA>        <NA>                    Milliken 2010
181           <NA>        <NA>        <NA>                    Milliken 2010
182           <NA>        <NA>        <NA>                    Milliken 2010
183           <NA>        <NA>        <NA>                    Milliken 2010
184           <NA>        <NA>        <NA>                    Milliken 2010
185           <NA>        <NA>        <NA>                    Milliken 2010
186           <NA>        <NA>        <NA>                    Milliken 2010
187           <NA>        <NA>        <NA>                    Milliken 2010
188           <NA>        <NA>        <NA>                    Milliken 2010
189           <NA>        <NA>        <NA>                    Milliken 2010
190           <NA>        <NA>        <NA>                    Jepson Online
191           <NA>        <NA>        <NA>                    Jepson Online
192           <NA>        <NA>        <NA>                    Jepson Online
193           <NA>        <NA>        <NA>                    Jepson Online
194           <NA>        <NA>        <NA>                    Jepson Online
195           <NA>        <NA>        <NA>                    Jepson Online
196           <NA>        <NA>        <NA>                    Jepson Online
197           <NA>        <NA>        <NA>                    Jepson Online
198           <NA>        <NA>        <NA>                    Jepson Online
199           <NA>        <NA>        <NA>                    Jepson Online
200           <NA>        <NA>        <NA>                    Jepson Online
201           <NA>        <NA>        <NA>                    Jepson Online
202           <NA>        <NA>        <NA>                    Jepson Online
203           <NA>        <NA>        <NA>                    Jepson Online
204           <NA>        <NA>        <NA>                    Jepson Online
205           <NA>        <NA>        <NA>                    Jepson Online
206           <NA>        <NA>        <NA>                    Jepson Online
207           <NA>        <NA>        <NA>                    Jepson Online
208           <NA>        <NA>        <NA>                    Jepson Online
209           <NA>        <NA>        <NA>                    Jepson Online
210           <NA>        <NA>        <NA>                    Jepson Online
211           <NA>        <NA>        <NA>                    Jepson Online
212           <NA>        <NA>        <NA>                    Jepson Online
213           <NA>        <NA>        <NA>                    Jepson Online
214           <NA>        <NA>        <NA>                    Jepson Online
215           <NA>        <NA>        <NA>                    Jepson Online
216           <NA>        <NA>        <NA>                    Jepson Online
217           <NA>        <NA>        <NA>                    Jepson Online
218           <NA>        <NA>        <NA>                    Jepson Online
219           <NA>        <NA>        <NA>                    Jepson Online
220           <NA>        <NA>        <NA>                    Jepson Online
221           <NA>        <NA>        <NA>                    Jepson Online
222           <NA>        <NA>        <NA>                    Jepson Online
223           <NA>        <NA>        <NA>                    Jepson Online
224           <NA>        <NA>        <NA>                    Jepson Online
225           <NA>        <NA>        <NA>                    Jepson Online
226           <NA>        <NA>        <NA>                    Jepson Online
227           <NA>        <NA>        <NA>                    Jepson Online
228           <NA>        <NA>        <NA>                    Milliken 2010
229           <NA>        <NA>        <NA>                    Milliken 2010
230           <NA>        <NA>        <NA>                    Milliken 2010
231           <NA>        <NA>        <NA>                    Milliken 2010
232           <NA>        <NA>        <NA>                    Milliken 2010
233           <NA>        <NA>        <NA>                    Milliken 2010
234           <NA>        <NA>        <NA>                    Milliken 2010
235           <NA>        <NA>        <NA>                    Milliken 2010
236           <NA>        <NA>        <NA>                    Milliken 2010
237           <NA>        <NA>        <NA>                    Milliken 2010
238           <NA>        <NA>        <NA>                    Milliken 2010
239           <NA>        <NA>        <NA>                    Milliken 2010
240           <NA>        <NA>        <NA>                    Milliken 2010
241           <NA>        <NA>        <NA>                    Milliken 2010
242           <NA>        <NA>        <NA>                    Milliken 2010
243           <NA>        <NA>        <NA>                    Milliken 2010
244           <NA>        <NA>        <NA>                    Milliken 2010
245           <NA>        <NA>        <NA>                    Milliken 2010
246           <NA>        <NA>        <NA>                    Milliken 2010
247           <NA>        <NA>        <NA>                    Milliken 2010
248           <NA>        <NA>        <NA>                    Milliken 2010
249           <NA>        <NA>        <NA>                    Milliken 2010
250           <NA>        <NA>        <NA>                    Milliken 2010
251           <NA>        <NA>        <NA>                    Milliken 2010
252           <NA>        <NA>        <NA>                    Milliken 2010
253           <NA>        <NA>        <NA>                    Milliken 2010
254           <NA>        <NA>        <NA>                    Milliken 2010
255           <NA>        <NA>        <NA>                    Milliken 2010
256           <NA>        <NA>        <NA>                    Milliken 2010
257           <NA>        <NA>        <NA>                    Milliken 2010
258           <NA>        <NA>        <NA>                    Milliken 2010
259           <NA>        <NA>        <NA>                    Milliken 2010
260           <NA>        <NA>        <NA>                    Milliken 2010
261           <NA>        <NA>        <NA>                    Milliken 2010
262           <NA>        <NA>        <NA>                    Milliken 2010
263           <NA>        <NA>        <NA>                    Milliken 2010
264           <NA>        <NA>        <NA>                    Milliken 2010
265           <NA>        <NA>        <NA>                    Milliken 2010
266           <NA>        <NA>        <NA>                    Milliken 2010
267           <NA>        <NA>        <NA>                    Milliken 2010
268           <NA>        <NA>        <NA>                    Milliken 2010
269           <NA>        <NA>        <NA>                    Milliken 2010
270           <NA>        <NA>        <NA>                    Milliken 2010
271           <NA>        <NA>        <NA>                    Milliken 2010
272           <NA>        <NA>        <NA>                    Milliken 2010
273           <NA>        <NA>        <NA>                    Jepson Online
274           <NA>        <NA>        <NA>                    Jepson Online
275           <NA>        <NA>        <NA>                    Jepson Online
276           <NA>        <NA>        <NA>                    Jepson Online
277           <NA>        <NA>        <NA>                    Jepson Online
278           <NA>        <NA>        <NA>                    Jepson Online
279           <NA>        <NA>        <NA>                    Jepson Online
280           <NA>        <NA>        <NA>                    Jepson Online
281           <NA>        <NA>        <NA>                    Jepson Online
282           <NA>        <NA>        <NA>                    Milliken 2010
283           <NA>        <NA>        <NA>                    Milliken 2010
284           <NA>        <NA>        <NA>                    Milliken 2010
285           <NA>        <NA>        <NA>                    Milliken 2010
286           <NA>        <NA>        <NA>                    Milliken 2010
287           <NA>        <NA>        <NA>                    Milliken 2010
288           <NA>        <NA>        <NA>                    Milliken 2010
289           <NA>        <NA>        <NA>                    Milliken 2010
290           <NA>        <NA>        <NA>                    Milliken 2010
291           <NA>        <NA>        <NA>            Porter,  Johnson 2000
292           <NA>        <NA>        <NA>            Porter,  Johnson 2000
293           <NA>        <NA>        <NA>                    Jepson Online
294           <NA>        <NA>        <NA>                    Jepson Online
295           <NA>        <NA>        <NA>                    Jepson Online
296           <NA>        <NA>        <NA>                    Jepson Online
297           <NA>        <NA>        <NA>                    Jepson Online
298           <NA>        <NA>        <NA>                    Jepson Online
299           <NA>        <NA>        <NA>                    Jepson Online
300           <NA>        <NA>        <NA>                    Jepson Online
301           <NA>        <NA>        <NA>                    Jepson Online
302           <NA>        <NA>        <NA>                    Jepson Online
303           <NA>        <NA>        <NA>                    Jepson Online
304           <NA>        <NA>        <NA>                    Jepson Online
305           <NA>        <NA>        <NA>                    Jepson Online
306           <NA>        <NA>        <NA>                    Jepson Online
307           <NA>        <NA>        <NA>                    Jepson Online
308           <NA>        <NA>        <NA>                    Jepson Online
309           <NA>        <NA>        <NA>                    Jepson Online
310           <NA>        <NA>        <NA>                    Jepson Online
311           <NA>        <NA>        <NA>                    Jepson Online
312           <NA>        <NA>        <NA>                    Jepson Online
313           <NA>        <NA>        <NA>                    Jepson Online
314           <NA>        <NA>        <NA>                    Jepson Online
315           <NA>        <NA>        <NA>                    Jepson Online
316           <NA>        <NA>        <NA>                    Jepson Online
317           <NA>        <NA>        <NA>                    Jepson Online
318           <NA>        <NA>        <NA>                    Jepson Online
319           <NA>        <NA>        <NA>                    Jepson Online
320           <NA>        <NA>        <NA>                    Jepson Online
321           <NA>        <NA>        <NA>                    Jepson Online
322           <NA>        <NA>        <NA>                    Jepson Online
323           <NA>        <NA>        <NA>                    Jepson Online
324           <NA>        <NA>        <NA>                    Jepson Online
325           <NA>        <NA>        <NA>                    Jepson Online
326           <NA>        <NA>        <NA>                    Jepson Online
327           <NA>        <NA>        <NA>                    Jepson Online
328           <NA>        <NA>        <NA>                    Jepson Online
329           <NA>        <NA>        <NA>                    Jepson Online
330           <NA>        <NA>        <NA>                    Jepson Online
331           <NA>        <NA>        <NA>                    Jepson Online
332           <NA>        <NA>        <NA>                    Jepson Online
333           <NA>        <NA>        <NA>                    Jepson Online
334           <NA>        <NA>        <NA>                    Jepson Online
335           <NA>        <NA>        <NA>                    Jepson Online
336           <NA>        <NA>        <NA>                    Jepson Online
337           <NA>        <NA>        <NA>                    Jepson Online
338           <NA>        <NA>        <NA>                    Jepson Online
339           <NA>        <NA>        <NA>                    Jepson Online
340           <NA>        <NA>        <NA>                    Jepson Online
341           <NA>        <NA>        <NA>                    Jepson Online
342           <NA>        <NA>        <NA>                    Jepson Online
343           <NA>        <NA>        <NA>                    Jepson Online
344           <NA>        <NA>        <NA>                    Jepson Online
345           <NA>        <NA>        <NA>                    Jepson Online
346           <NA>        <NA>        <NA>                    Jepson Online
347           <NA>        <NA>        <NA>                    Jepson Online
348           <NA>        <NA>        <NA>                    Jepson Online
349           <NA>        <NA>        <NA>                    Jepson Online
350           <NA>        <NA>        <NA>                    Jepson Online
351           <NA>        <NA>        <NA>                    Jepson Online
352           <NA>        <NA>        <NA>                    Jepson Online
353           <NA>        <NA>        <NA>                    Jepson Online
354           <NA>        <NA>        <NA>                    Jepson Online
355           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
356           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
357           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
358           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
359           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
360           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
361           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
362           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
363           <NA>        <NA>        <NA>     Arizona-Sonora Desert Museum
364           <NA>        <NA>        <NA>                    Jepson Online
365           <NA>        <NA>        <NA>                    Jepson Online
366           <NA>        <NA>        <NA>                    Jepson Online
367           <NA>        <NA>        <NA>                    Jepson Online
368           <NA>        <NA>        <NA>                    Jepson Online
369           <NA>        <NA>        <NA>                    Jepson Online
370           <NA>        <NA>        <NA>                    Jepson Online
371           <NA>        <NA>        <NA>                    Jepson Online
372           <NA>        <NA>        <NA>                    Jepson Online
373           <NA>        <NA>        <NA>                    Jepson Online
374           <NA>        <NA>        <NA>                    Jepson Online
375           <NA>        <NA>        <NA>                    Jepson Online
376           <NA>        <NA>        <NA>                    Jepson Online
377           <NA>        <NA>        <NA>                    Jepson Online
378           <NA>        <NA>        <NA>                    Jepson Online
379           <NA>        <NA>        <NA>                    Jepson Online
380           <NA>        <NA>        <NA>                    Jepson Online
381           <NA>        <NA>        <NA>                    Jepson Online
382           <NA>        <NA>        <NA>                    Jepson Online
383           <NA>        <NA>        <NA>                    Jepson Online
384           <NA>        <NA>        <NA>                    Jepson Online
385           <NA>        <NA>        <NA>                    Jepson Online
386           <NA>        <NA>        <NA>                    Jepson Online
387           <NA>        <NA>        <NA>                    Jepson Online
388           <NA>        <NA>        <NA>                    Jepson Online
389           <NA>        <NA>        <NA>                    Jepson Online
390           <NA>        <NA>        <NA>                    Jepson Online
391           <NA>        <NA>        <NA>                    Jepson Online
392           <NA>        <NA>        <NA>                    Jepson Online
393           <NA>        <NA>        <NA>                    Jepson Online
394           <NA>        <NA>        <NA>                    Jepson Online
395           <NA>        <NA>        <NA>                    Jepson Online
396           <NA>        <NA>        <NA>                    Jepson Online
397           <NA>        <NA>        <NA>                Grant  Grant 1965
398           <NA>        <NA>        <NA>                Grant  Grant 1965
399           <NA>        <NA>        <NA>                Grant  Grant 1965
400           <NA>        <NA>        <NA>                Grant  Grant 1965
401           <NA>        <NA>        <NA>                Grant  Grant 1965
402           <NA>        <NA>        <NA>                Grant  Grant 1965
403           <NA>        <NA>        <NA>                Grant  Grant 1965
404           <NA>        <NA>        <NA>                Grant  Grant 1965
405           <NA>        <NA>        <NA>                Grant  Grant 1965
406           <NA>        <NA>        <NA>                Grant  Grant 1965
407           <NA>        <NA>        <NA>                Grant  Grant 1965
408           <NA>        <NA>        <NA>                Grant  Grant 1965
409           <NA>        <NA>        <NA>                Grant  Grant 1965
410           <NA>        <NA>        <NA>                Grant  Grant 1965
411           <NA>        <NA>        <NA>                Grant  Grant 1965
412           <NA>        <NA>        <NA>                Grant  Grant 1965
413           <NA>        <NA>        <NA>                Grant  Grant 1965
414           <NA>        <NA>        <NA>                Grant  Grant 1965
415           <NA>        <NA>        <NA>                Grant  Grant 1965
416           <NA>        <NA>        <NA>                Grant  Grant 1965
417           <NA>        <NA>        <NA>                Grant  Grant 1965
418           <NA>        <NA>        <NA>                Grant  Grant 1965
419           <NA>        <NA>        <NA>                Grant  Grant 1965
420           <NA>        <NA>        <NA>                Grant  Grant 1965
421           <NA>        <NA>        <NA>                    Jepson Online
422           <NA>        <NA>        <NA>                    Jepson Online
423           <NA>        <NA>        <NA>                    Jepson Online
424           <NA>        <NA>        <NA>                    Jepson Online
425           <NA>        <NA>        <NA>                    Jepson Online
426           <NA>        <NA>        <NA>                    Jepson Online
427           <NA>        <NA>        <NA>                    Jepson Online
428           <NA>        <NA>        <NA>                    Jepson Online
429           <NA>        <NA>        <NA>                    Jepson Online
430           <NA>        <NA>        <NA>                    Jepson Online
431           <NA>        <NA>        <NA>                    Jepson Online
432           <NA>        <NA>        <NA>                    Jepson Online
433           <NA>        <NA>        <NA>                  Flora Mendocina
434           <NA>        <NA>        <NA>                  Flora Mendocina
435           <NA>        <NA>        <NA>                  Flora Mendocina
436           <NA>        <NA>        <NA>                  Flora Mendocina
437           <NA>        <NA>        <NA>                  Flora Mendocina
438           <NA>        <NA>        <NA>                  Flora Mendocina
439           <NA>        <NA>        <NA>                  Flora Mendocina
440           <NA>        <NA>        <NA>                  Flora Mendocina
441           <NA>        <NA>        <NA>                  Flora Mendocina
442           <NA>        <NA>        <NA>                    Jepson Online
443           <NA>        <NA>        <NA>                    Jepson Online
444           <NA>        <NA>        <NA>                    Jepson Online
445           <NA>        <NA>        <NA>                    Jepson Online
446           <NA>        <NA>        <NA>                    Jepson Online
447           <NA>        <NA>        <NA>                    Jepson Online
448           <NA>        <NA>        <NA>                    Jepson Online
449           <NA>        <NA>        <NA>                    Jepson Online
450           <NA>        <NA>        <NA>                    Jepson Online
451           <NA>        <NA>        <NA>                    Jepson Online
452           <NA>        <NA>        <NA>                    Jepson Online
453           <NA>        <NA>        <NA>                    Jepson Online
454           <NA>        <NA>        <NA>                    Jepson Online
455           <NA>        <NA>        <NA>                    Jepson Online
456           <NA>        <NA>        <NA>                    Jepson Online
457           <NA>        <NA>        <NA>                    Jepson Online
458           <NA>        <NA>        <NA>                    Jepson Online
459           <NA>        <NA>        <NA>                    Jepson Online
460           <NA>        <NA>        <NA>                    Jepson Online
461           <NA>        <NA>        <NA>                    Jepson Online
462           <NA>        <NA>        <NA>                    Jepson Online
463           <NA>        <NA>        <NA>                    Jepson Online
464           <NA>        <NA>        <NA>                    Jepson Online
465           <NA>        <NA>        <NA>                    Jepson Online
466           <NA>        <NA>        <NA>                    Jepson Online
467           <NA>        <NA>        <NA>                    Jepson Online
468           <NA>        <NA>        <NA>                    Jepson Online
469           <NA>        <NA>        <NA>                    Jepson Online
470           <NA>        <NA>        <NA>                    Jepson Online
471           <NA>        <NA>        <NA>                    Jepson Online
472           <NA>        <NA>        <NA>                    Jepson Online
473           <NA>        <NA>        <NA>                    Jepson Online
474           <NA>        <NA>        <NA>                    Jepson Online
475           <NA>        <NA>        <NA>                    Jepson Online
476           <NA>        <NA>        <NA>                    Jepson Online
477           <NA>        <NA>        <NA>                    Jepson Online
478           <NA>        <NA>        <NA>                    Jepson Online
479           <NA>        <NA>        <NA>                    Jepson Online
480           <NA>        <NA>        <NA>                    Jepson Online
481           <NA>        <NA>        <NA>                    Jepson Online
482           <NA>        <NA>        <NA>                    Jepson Online
483           <NA>        <NA>        <NA>                    Jepson Online
484           <NA>        <NA>        <NA>                    Jepson Online
485           <NA>        <NA>        <NA>                    Jepson Online
486           <NA>        <NA>        <NA>                    Jepson Online
487           <NA>        <NA>        <NA>                    Jepson Online
488           <NA>        <NA>        <NA>                    Jepson Online
489           <NA>        <NA>        <NA>                    Jepson Online
490           <NA>        <NA>        <NA>                    Jepson Online
491           <NA>        <NA>        <NA>                    Jepson Online
492           <NA>        <NA>        <NA>                    Jepson Online
493           <NA>        <NA>        <NA>                    Jepson Online
494           <NA>        <NA>        <NA>                    Jepson Online
495           <NA>        <NA>        <NA>        Herbarium sheet MO2926797
496           <NA>        <NA>        <NA>        Herbarium sheet MO2926797
497           <NA>        <NA>        <NA>        Herbarium sheet MO2926797
498           <NA>        <NA>        <NA>                    Jepson Online
499           <NA>        <NA>        <NA>                    Jepson Online
500           <NA>        <NA>        <NA>                    Jepson Online
501           <NA>        <NA>        <NA>                    Jepson Online
502           <NA>        <NA>        <NA>                    Jepson Online
503           <NA>        <NA>        <NA>                    Jepson Online
504           <NA>        <NA>        <NA>                    Jepson Online
505           <NA>        <NA>        <NA>                    Jepson Online
506           <NA>        <NA>        <NA>                    Jepson Online
507           <NA>        <NA>        <NA>                    Jepson Online
508           <NA>        <NA>        <NA>                    Jepson Online
509           <NA>        <NA>        <NA>                    Jepson Online
510           <NA>        <NA>        <NA>                    Jepson Online
511         purple        <NA>        <NA>                    Jepson Online
512         purple        <NA>        <NA>                    Jepson Online
513         purple        <NA>        <NA>                    Jepson Online
514         purple        <NA>        <NA>                    Jepson Online
515         purple        <NA>        <NA>                    Jepson Online
516         purple        <NA>        <NA>                    Jepson Online
517         purple        <NA>        <NA>                    Jepson Online
518         purple        <NA>        <NA>                    Jepson Online
519         purple        <NA>        <NA>                    Jepson Online
520         purple        <NA>        <NA>                    Jepson Online
521         purple        <NA>        <NA>                    Jepson Online
522         purple        <NA>        <NA>                    Jepson Online
523         purple        <NA>        <NA>                    Jepson Online
524         purple        <NA>        <NA>                    Jepson Online
525         purple        <NA>        <NA>                    Jepson Online
526           <NA>        <NA>        <NA>                    Jepson Online
527           <NA>        <NA>        <NA>                    Jepson Online
528           <NA>        <NA>        <NA>                    Jepson Online
529           <NA>        <NA>        <NA>                    Jepson Online
530           <NA>        <NA>        <NA>                    Jepson Online
531           <NA>        <NA>        <NA>                    Jepson Online
532           <NA>        <NA>        <NA>                    Jepson Online
533           <NA>        <NA>        <NA>                    Jepson Online
534           <NA>        <NA>        <NA>                    Jepson Online
535           <NA>        <NA>        <NA>                    Jepson Online
536           <NA>        <NA>        <NA>                    Jepson Online
537           <NA>        <NA>        <NA>                    Jepson Online
538         purple        <NA>        <NA>                Grant  Grant 1965
539         purple        <NA>        <NA>                Grant  Grant 1965
540         purple        <NA>        <NA>                Grant  Grant 1965
541         purple        <NA>        <NA>                Grant  Grant 1965
542         purple        <NA>        <NA>                Grant  Grant 1965
543         purple        <NA>        <NA>                Grant  Grant 1965
544         purple        <NA>        <NA>                Grant  Grant 1965
545         purple        <NA>        <NA>                Grant  Grant 1965
546         purple        <NA>        <NA>                Grant  Grant 1965
547         purple        <NA>        <NA>                Grant  Grant 1965
548         purple        <NA>        <NA>                Grant  Grant 1965
549         purple        <NA>        <NA>                Grant  Grant 1965
550         purple        <NA>        <NA>                Grant  Grant 1965
551         purple        <NA>        <NA>                Grant  Grant 1965
552         purple        <NA>        <NA>                Grant  Grant 1965
553         purple        <NA>        <NA>                Grant  Grant 1965
554         purple        <NA>        <NA>                Grant  Grant 1965
555         purple        <NA>        <NA>                Grant  Grant 1965
556         purple        <NA>        <NA>                    Jepson Online
557         purple        <NA>        <NA>                    Jepson Online
558         purple        <NA>        <NA>                    Jepson Online
559           <NA>        <NA>        <NA>                Grant  Grant 1965
560           <NA>        <NA>        <NA>                Grant  Grant 1965
561           <NA>        <NA>        <NA>                Grant  Grant 1965
562           <NA>        <NA>        <NA>                Grant  Grant 1965
563           <NA>        <NA>        <NA>                Grant  Grant 1965
564           <NA>        <NA>        <NA>                Grant  Grant 1965
565           <NA>        <NA>        <NA>                Grant  Grant 1965
566           <NA>        <NA>        <NA>                Grant  Grant 1965
567           <NA>        <NA>        <NA>                Grant  Grant 1965
568           <NA>        <NA>        <NA>                Grant  Grant 1965
569           <NA>        <NA>        <NA>                Grant  Grant 1965
570           <NA>        <NA>        <NA>                Grant  Grant 1965
571           <NA>        <NA>        <NA>                Grant  Grant 1965
572           <NA>        <NA>        <NA>                Grant  Grant 1965
573           <NA>        <NA>        <NA>                Grant  Grant 1965
574           <NA>        <NA>        <NA>              Montana Field Guide
575           <NA>        <NA>        <NA>              Montana Field Guide
576           <NA>        <NA>        <NA>              Montana Field Guide
577           <NA>        <NA>        <NA>              Montana Field Guide
578           <NA>        <NA>        <NA>              Montana Field Guide
579           <NA>        <NA>        <NA>              Montana Field Guide
580           <NA>        <NA>        <NA>              Montana Field Guide
581           <NA>        <NA>        <NA>              Montana Field Guide
582           <NA>        <NA>        <NA>              Montana Field Guide
583           <NA>        <NA>        <NA>        Chileflora Plant Database
584           <NA>        <NA>        <NA>        Chileflora Plant Database
585           <NA>        <NA>        <NA>        Chileflora Plant Database
586           <NA>        <NA>        <NA>        Chileflora Plant Database
587           <NA>        <NA>        <NA>        Chileflora Plant Database
588           <NA>        <NA>        <NA>                Grant  Grant 1965
589           <NA>        <NA>        <NA>                Grant  Grant 1965
590           <NA>        <NA>        <NA>                Grant  Grant 1965
591           <NA>        <NA>        <NA>                Grant  Grant 1965
592           <NA>        <NA>        <NA>                Grant  Grant 1965
593           <NA>        <NA>        <NA>                Grant  Grant 1965
594           <NA>        <NA>        <NA>                Grant  Grant 1965
595           <NA>        <NA>        <NA>                Grant  Grant 1965
596           <NA>        <NA>        <NA>                Grant  Grant 1965
597           <NA>        <NA>        <NA>                Grant  Grant 1965
598           <NA>        <NA>        <NA>                Grant  Grant 1965
599           <NA>        <NA>        <NA>                Grant  Grant 1965
600           <NA>        <NA>        <NA>                Grant  Grant 1965
601           <NA>        <NA>        <NA>                Grant  Grant 1965
602           <NA>        <NA>        <NA>                Grant  Grant 1965
603           <NA>        <NA>        <NA>                Grant  Grant 1965
604           <NA>        <NA>        <NA>                Grant  Grant 1965
605           <NA>        <NA>        <NA>                Grant  Grant 1965
606           <NA>        <NA>        <NA>                Grant  Grant 1965
607           <NA>        <NA>        <NA>                Grant  Grant 1965
608           <NA>        <NA>        <NA>                Grant  Grant 1965
609           <NA>        <NA>        <NA>                Grant  Grant 1965
610           <NA>        <NA>        <NA>                Grant  Grant 1965
611  yellow throat        <NA>        <NA>                Grant  Grant 1965
612  yellow throat        <NA>        <NA>                Grant  Grant 1965
613  yellow throat        <NA>        <NA>                Grant  Grant 1965
614           <NA>        <NA>        <NA>                    Jepson Online
615           <NA>        <NA>        <NA>                    Jepson Online
616           <NA>        <NA>        <NA>                    Jepson Online
617           <NA>        <NA>        <NA>                    Jepson Online
618           <NA>        <NA>        <NA>                    Jepson Online
619           <NA>        <NA>        <NA>                    Jepson Online
620           <NA>        <NA>        <NA>                Grant  Grant 1965
621           <NA>        <NA>        <NA>                Grant  Grant 1965
622           <NA>        <NA>        <NA>                Grant  Grant 1965
623           <NA>        <NA>        <NA>                Grant  Grant 1965
624           <NA>        <NA>        <NA>                Grant  Grant 1965
625           <NA>        <NA>        <NA>                Grant  Grant 1965
626           <NA>        <NA>        <NA>                Grant  Grant 1965
627           <NA>        <NA>        <NA>                Grant  Grant 1965
628           <NA>        <NA>        <NA>                Grant  Grant 1965
629           <NA>        <NA>        <NA>                Grant  Grant 1965
630           <NA>        <NA>        <NA>                Grant  Grant 1965
631           <NA>        <NA>        <NA>                Grant  Grant 1965
632           <NA>        <NA>        <NA>                Grant  Grant 1965
633           <NA>        <NA>        <NA>                Grant  Grant 1965
634           <NA>        <NA>        <NA>                Grant  Grant 1965
635           <NA>        <NA>        <NA>                Grant  Grant 1965
636           <NA>        <NA>        <NA>                Grant  Grant 1965
637           <NA>        <NA>        <NA>                Grant  Grant 1965
638           <NA>        <NA>        <NA>                Grant  Grant 1965
639           <NA>        <NA>        <NA>                Grant  Grant 1965
640           <NA>        <NA>        <NA>                Grant  Grant 1965
641           <NA>        <NA>        <NA>                Grant  Grant 1965
642           <NA>        <NA>        <NA>                Grant  Grant 1965
643           <NA>        <NA>        <NA>                Grant  Grant 1965
644           <NA>        <NA>        <NA>                Grant  Grant 1965
645           <NA>        <NA>        <NA>                Grant  Grant 1965
646           <NA>        <NA>        <NA>                Grant  Grant 1965
647           <NA>        <NA>        <NA>                Grant  Grant 1965
648           <NA>        <NA>        <NA>                Grant  Grant 1965
649           <NA>        <NA>        <NA>                Grant  Grant 1965
650           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
651           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
652           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
653           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
654           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
655           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
656           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
657           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
658           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
659           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
660           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
661           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
662           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
663           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
664           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
665           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
666           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
667           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
668           <NA>        <NA>        <NA>                Grant  Grant 1965
669           <NA>        <NA>        <NA>                Grant  Grant 1965
670           <NA>        <NA>        <NA>                Grant  Grant 1965
671           <NA>        <NA>        <NA>                Grant  Grant 1965
672           <NA>        <NA>        <NA>                Grant  Grant 1965
673           <NA>        <NA>        <NA>                Grant  Grant 1965
674           <NA>        <NA>        <NA>                Grant  Grant 1965
675           <NA>        <NA>        <NA>                Grant  Grant 1965
676           <NA>        <NA>        <NA>                Grant  Grant 1965
677           <NA>        <NA>        <NA>                Grant  Grant 1965
678           <NA>        <NA>        <NA>                Grant  Grant 1965
679           <NA>        <NA>        <NA>                Grant  Grant 1965
680           <NA>        <NA>        <NA>                Grant  Grant 1965
681           <NA>        <NA>        <NA>                Grant  Grant 1965
682           <NA>        <NA>        <NA>                Grant  Grant 1965
683           <NA>        <NA>        <NA>                Grant  Grant 1965
684           <NA>        <NA>        <NA>                Grant  Grant 1965
685           <NA>        <NA>        <NA>                Grant  Grant 1965
686           <NA>        <NA>        <NA>                Grant  Grant 1965
687           <NA>        <NA>        <NA>                Grant  Grant 1965
688           <NA>        <NA>        <NA>                Grant  Grant 1965
689           <NA>        <NA>        <NA>                Grant  Grant 1965
690           <NA>        <NA>        <NA>                Grant  Grant 1965
691           <NA>        <NA>        <NA>                Grant  Grant 1965
692           <NA>        <NA>        <NA>                Grant  Grant 1965
693           <NA>        <NA>        <NA>                Grant  Grant 1965
694           <NA>        <NA>        <NA>                Grant  Grant 1965
695           <NA>        <NA>        <NA>                Grant  Grant 1965
696           <NA>        <NA>        <NA>                Grant  Grant 1965
697           <NA>        <NA>        <NA>                Grant  Grant 1965
698           <NA>        <NA>        <NA>                Grant  Grant 1965
699           <NA>        <NA>        <NA>                Grant  Grant 1965
700           <NA>        <NA>        <NA>                Grant  Grant 1965
701           <NA>        <NA>        <NA>                Grant  Grant 1965
702           <NA>        <NA>        <NA>                Grant  Grant 1965
703           <NA>        <NA>        <NA>                Grant  Grant 1965
704           <NA>        <NA>        <NA>                Grant  Grant 1965
705           <NA>        <NA>        <NA>                Grant  Grant 1965
706           <NA>        <NA>        <NA>                Grant  Grant 1965
707           <NA>        <NA>        <NA>                Grant  Grant 1965
708           <NA>        <NA>        <NA>                Grant  Grant 1965
709           <NA>        <NA>        <NA>                Grant  Grant 1965
710           <NA>        <NA>        <NA>                Grant  Grant 1965
711           <NA>        <NA>        <NA>                Grant  Grant 1965
712           <NA>        <NA>        <NA>                Grant  Grant 1965
713           <NA>        <NA>        <NA>                Grant  Grant 1965
714           <NA>        <NA>        <NA>                Grant  Grant 1965
715           <NA>        <NA>        <NA>                Grant  Grant 1965
716           <NA>        <NA>        <NA>                Grant  Grant 1965
717           <NA>        <NA>        <NA>                Grant  Grant 1965
718           <NA>        <NA>        <NA>                Grant  Grant 1965
719           <NA>        <NA>        <NA>                Grant  Grant 1965
720           <NA>        <NA>        <NA>                Grant  Grant 1965
721           <NA>        <NA>        <NA>                Grant  Grant 1965
722           <NA>        <NA>        <NA>                Grant  Grant 1965
723           <NA>        <NA>        <NA>                Grant  Grant 1965
724           <NA>        <NA>        <NA>                Grant  Grant 1965
725           <NA>        <NA>        <NA>                Grant  Grant 1965
726           <NA>        <NA>        <NA>                Grant  Grant 1965
727           <NA>        <NA>        <NA>                Grant  Grant 1965
728           <NA>        <NA>        <NA>                Grant  Grant 1965
729           <NA>        <NA>        <NA>                Grant  Grant 1965
730           <NA>        <NA>        <NA>                Grant  Grant 1965
731           <NA>        <NA>        <NA>                Grant  Grant 1965
732           <NA>        <NA>        <NA>                Grant  Grant 1965
733           <NA>        <NA>        <NA>                Grant  Grant 1965
734           <NA>        <NA>        <NA>                Grant  Grant 1965
735           <NA>        <NA>        <NA>                Grant  Grant 1965
736           <NA>        <NA>        <NA>                Grant  Grant 1965
737           <NA>        <NA>        <NA>                Grant  Grant 1965
738           <NA>        <NA>        <NA>                Grant  Grant 1965
739           <NA>        <NA>        <NA>                Grant  Grant 1965
740           <NA>        <NA>        <NA>                Grant  Grant 1965
741           <NA>        <NA>        <NA>                Grant  Grant 1965
742           <NA>        <NA>        <NA>                Grant  Grant 1965
743           <NA>        <NA>        <NA>                Grant  Grant 1965
744           <NA>        <NA>        <NA>                Grant  Grant 1965
745           <NA>        <NA>        <NA>                Grant  Grant 1965
746           <NA>        <NA>        <NA>                    Jepson Online
747           <NA>        <NA>        <NA>                    Jepson Online
748           <NA>        <NA>        <NA>                    Jepson Online
749           <NA>        <NA>        <NA>                    Jepson Online
750           <NA>        <NA>        <NA>                    Jepson Online
751           <NA>        <NA>        <NA>                    Jepson Online
752           <NA>        <NA>        <NA>                    Jepson Online
753           <NA>        <NA>        <NA>                    Jepson Online
754           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
755           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
756           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
757           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
758           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
759           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
760           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
761           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
762           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
763           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
764           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
765           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
766           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
767           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
768           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
769           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
770           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
771           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
772           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
773           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
774           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
775           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
776           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
777           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
778           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
779           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
780           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
781           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
782           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
783           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
784           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
785           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
786           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
787           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
788           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
789           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
790           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
791           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
792           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
793           <NA>        <NA>        <NA>                Grant  Grant 1965
794           <NA>        <NA>        <NA>                Grant  Grant 1965
795           <NA>        <NA>        <NA>                Grant  Grant 1965
796           <NA>        <NA>        <NA>                Grant  Grant 1965
797           <NA>        <NA>        <NA>                Grant  Grant 1965
798           <NA>        <NA>        <NA>                Grant  Grant 1965
799           <NA>        <NA>        <NA>                Grant  Grant 1965
800           <NA>        <NA>        <NA>                Grant  Grant 1965
801           <NA>        <NA>        <NA>                Grant  Grant 1965
802           <NA>        <NA>        <NA>                Grant  Grant 1965
803           <NA>        <NA>        <NA>                Grant  Grant 1965
804           <NA>        <NA>        <NA>                Grant  Grant 1965
805           <NA>        <NA>        <NA>                Grant  Grant 1965
806           <NA>        <NA>        <NA>                Grant  Grant 1965
807           <NA>        <NA>        <NA>                Grant  Grant 1965
808           <NA>        <NA>        <NA>                Grant  Grant 1965
809           <NA>        <NA>        <NA>                Grant  Grant 1965
810           <NA>        <NA>        <NA>                Grant  Grant 1965
811           <NA>        <NA>        <NA>                Grant  Grant 1965
812           <NA>        <NA>        <NA>                Grant  Grant 1965
813           <NA>        <NA>        <NA>                Grant  Grant 1965
814           <NA>        <NA>        <NA>                Grant  Grant 1965
815           <NA>        <NA>        <NA>                Grant  Grant 1965
816           <NA>        <NA>        <NA>                Grant  Grant 1965
817           <NA>        <NA>        <NA>                Grant  Grant 1965
818           <NA>        <NA>        <NA>                Grant  Grant 1965
819           <NA>        <NA>        <NA>                Grant  Grant 1965
820           <NA>        <NA>        <NA>                Grant  Grant 1965
821           <NA>        <NA>        <NA>                Grant  Grant 1965
822           <NA>        <NA>        <NA>                        CalPhotos
823           <NA>        <NA>        <NA>                        CalPhotos
824           <NA>        <NA>        <NA>                        CalPhotos
825           <NA>        <NA>        <NA>                        CalPhotos
826           <NA>        <NA>        <NA>                        CalPhotos
827           <NA>        <NA>        <NA>                        CalPhotos
828           <NA>        <NA>        <NA>                        CalPhotos
829           <NA>        <NA>        <NA>                        CalPhotos
830           <NA>        <NA>        <NA>                        CalPhotos
831           <NA>        <NA>        <NA>                        CalPhotos
832           <NA>        <NA>        <NA>                        CalPhotos
833           <NA>        <NA>        <NA>                        CalPhotos
834           <NA>        <NA>        <NA>                        CalPhotos
835           <NA>        <NA>        <NA>                        CalPhotos
836           <NA>        <NA>        <NA>                        CalPhotos
837           <NA>        <NA>        <NA>                        CalPhotos
838           <NA>        <NA>        <NA>                        CalPhotos
839           <NA>        <NA>        <NA>                        CalPhotos
840           <NA>        <NA>        <NA>                        CalPhotos
841           <NA>        <NA>        <NA>                        CalPhotos
842           <NA>        <NA>        <NA>                        CalPhotos
843           <NA>        <NA>        <NA>                        CalPhotos
844         purple        pink        <NA>                    Jepson Online
845         purple        pink        <NA>                    Jepson Online
846         purple        pink        <NA>                    Jepson Online
847         purple        pink        <NA>                    Jepson Online
848         purple        pink        <NA>                    Jepson Online
849         purple        pink        <NA>                    Jepson Online
850         purple        pink        <NA>                    Jepson Online
851         purple        pink        <NA>                    Jepson Online
852         purple        pink        <NA>                    Jepson Online
853           <NA>        <NA>        <NA>                    Jepson Online
854           <NA>        <NA>        <NA>                    Jepson Online
855           <NA>        <NA>        <NA>                    Jepson Online
856           <NA>        <NA>        <NA>                    Jepson Online
857           <NA>        <NA>        <NA>                    Jepson Online
858           <NA>        <NA>        <NA>                    Jepson Online
859           <NA>        <NA>        <NA>                    Jepson Online
860           <NA>        <NA>        <NA>                    Jepson Online
861           <NA>        <NA>        <NA>                    Jepson Online
862           <NA>        <NA>        <NA>                    Jepson Online
863           <NA>        <NA>        <NA>                    Jepson Online
864           <NA>        <NA>        <NA>                    Jepson Online
865           <NA>        <NA>        <NA>                    Jepson Online
866           <NA>        <NA>        <NA>                    Jepson Online
867           <NA>        <NA>        <NA>                    Jepson Online
868           <NA>        <NA>        <NA>                    Jepson Online
869           <NA>        <NA>        <NA>                    Jepson Online
870           <NA>        <NA>        <NA>                    Jepson Online
871           <NA>        <NA>        <NA>                    Jepson Online
872           <NA>        <NA>        <NA>                    Jepson Online
873           <NA>        <NA>        <NA>                    Jepson Online
874           <NA>        <NA>        <NA>                    Jepson Online
875           <NA>        <NA>        <NA>                    Jepson Online
876           <NA>        <NA>        <NA>                    Jepson Online
877           <NA>        <NA>        <NA>                    Jepson Online
878           <NA>        <NA>        <NA>                    Jepson Online
879           <NA>        <NA>        <NA>                    Jepson Online
880           <NA>        <NA>        <NA>                    Jepson Online
881           <NA>        <NA>        <NA>                    Jepson Online
882           <NA>        <NA>        <NA>                    Jepson Online
883           <NA>        <NA>        <NA>                    Jepson Online
884           <NA>        <NA>        <NA>                    Jepson Online
885           <NA>        <NA>        <NA>                    Jepson Online
886           <NA>        <NA>        <NA>                    Jepson Online
887           <NA>        <NA>        <NA>                    Jepson Online
888           <NA>        <NA>        <NA>                    Jepson Online
889           <NA>        <NA>        <NA>                    Jepson Online
890           <NA>        <NA>        <NA>                    Jepson Online
891           <NA>        <NA>        <NA>                    Jepson Online
892           <NA>        <NA>        <NA>                    Jepson Online
893           <NA>        <NA>        <NA>                    Jepson Online
894           <NA>        <NA>        <NA>                    Jepson Online
895           <NA>        <NA>        <NA>                    Jepson Online
896           <NA>        <NA>        <NA>                    Jepson Online
897           <NA>        <NA>        <NA>                    Jepson Online
898           pink      purple      yellow                    Jepson Online
899           pink      purple      yellow                    Jepson Online
900           pink      purple      yellow                    Jepson Online
901           pink      purple      yellow                    Jepson Online
902           pink      purple      yellow                    Jepson Online
903           pink      purple      yellow                    Jepson Online
904           <NA>        <NA>        <NA>                    Jepson Online
905           <NA>        <NA>        <NA>                    Jepson Online
906           <NA>        <NA>        <NA>                    Jepson Online
907           <NA>        <NA>        <NA>                    Jepson Online
908           <NA>        <NA>        <NA>                    Jepson Online
909           <NA>        <NA>        <NA>                    Jepson Online
910           <NA>        <NA>        <NA>                    Jepson Online
911           <NA>        <NA>        <NA>                    Jepson Online
912           <NA>        <NA>        <NA>                    Jepson Online
913           <NA>        <NA>        <NA>                    Jepson Online
914           <NA>        <NA>        <NA>                    Jepson Online
915           <NA>        <NA>        <NA>                    Jepson Online
916           <NA>        <NA>        <NA>                    Jepson Online
917           <NA>        <NA>        <NA>                    Jepson Online
918           <NA>        <NA>        <NA>                    Jepson Online
919           <NA>        <NA>        <NA>                    Jepson Online
920           <NA>        <NA>        <NA>                    Jepson Online
921           <NA>        <NA>        <NA>                    Jepson Online
922           <NA>        <NA>        <NA>                    Jepson Online
923           <NA>        <NA>        <NA>                    Jepson Online
924           <NA>        <NA>        <NA>                    Jepson Online
925           <NA>        <NA>        <NA>                    Jepson Online
926           <NA>        <NA>        <NA>                    Jepson Online
927           <NA>        <NA>        <NA>                    Jepson Online
928           <NA>        <NA>        <NA>                    Jepson Online
929           <NA>        <NA>        <NA>                    Jepson Online
930           <NA>        <NA>        <NA>                    Jepson Online
931           <NA>        <NA>        <NA>                    Jepson Online
932           <NA>        <NA>        <NA>                    Jepson Online
933           <NA>        <NA>        <NA>                    Jepson Online
934           <NA>        <NA>        <NA>                    Jepson Online
935           <NA>        <NA>        <NA>                    Jepson Online
936           <NA>        <NA>        <NA>                    Jepson Online
937           <NA>        <NA>        <NA>                    Jepson Online
938           <NA>        <NA>        <NA>                    Jepson Online
939           <NA>        <NA>        <NA>                    Jepson Online
940           <NA>        <NA>        <NA>                    Jepson Online
941           <NA>        <NA>        <NA>                    Jepson Online
942           <NA>        <NA>        <NA>                    Jepson Online
943           <NA>        <NA>        <NA>                    Jepson Online
944           <NA>        <NA>        <NA>                    Jepson Online
945           <NA>        <NA>        <NA>                    Jepson Online
946           <NA>        <NA>        <NA>                    Jepson Online
947           <NA>        <NA>        <NA>                    Jepson Online
948           <NA>        <NA>        <NA>                    Jepson Online
949           <NA>        <NA>        <NA>                    Jepson Online
950           <NA>        <NA>        <NA>                    Jepson Online
951           <NA>        <NA>        <NA>                    Jepson Online
952           <NA>        <NA>        <NA>                    Jepson Online
953           <NA>        <NA>        <NA>                    Jepson Online
954           <NA>        <NA>        <NA>                    Jepson Online
955   white (both)        <NA>        <NA>                    Jepson Online
956   white (both)        <NA>        <NA>                    Jepson Online
957   white (both)        <NA>        <NA>                    Jepson Online
958   white (both)        <NA>        <NA>                    Jepson Online
959   white (both)        <NA>        <NA>                    Jepson Online
960   white (both)        <NA>        <NA>                    Jepson Online
961           <NA>        <NA>        <NA>                Grant  Grant 1965
962           <NA>        <NA>        <NA>                Grant  Grant 1965
963           <NA>        <NA>        <NA>                Grant  Grant 1965
964           <NA>        <NA>        <NA>                    Jepson Online
965           <NA>        <NA>        <NA>                    Jepson Online
966           <NA>        <NA>        <NA>                    Jepson Online
967           <NA>        <NA>        <NA>                    Jepson Online
968           <NA>        <NA>        <NA>                    Jepson Online
969           <NA>        <NA>        <NA>                    Jepson Online
970           <NA>        <NA>        <NA>                Grant  Grant 1965
971           <NA>        <NA>        <NA>                Grant  Grant 1965
972           <NA>        <NA>        <NA>                Grant  Grant 1965
973           <NA>        <NA>        <NA>                Grant  Grant 1965
974           <NA>        <NA>        <NA>                Grant  Grant 1965
975           <NA>        <NA>        <NA>                Grant  Grant 1965
976           <NA>        <NA>        <NA>                Grant  Grant 1965
977           <NA>        <NA>        <NA>                Grant  Grant 1965
978           <NA>        <NA>        <NA>                Grant  Grant 1965
979           <NA>        <NA>        <NA>                Grant  Grant 1965
980           <NA>        <NA>        <NA>                Grant  Grant 1965
981           <NA>        <NA>        <NA>                Grant  Grant 1965
982           <NA>        <NA>        <NA>                Grant  Grant 1965
983           <NA>        <NA>        <NA>                Grant  Grant 1965
984           <NA>        <NA>        <NA>                Grant  Grant 1965
985           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
986           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
987           <NA>        <NA>        <NA>  Thomas Juenger Lab, Univ. Texas
988           <NA>        <NA>        <NA>                    Locklear 2011
989           <NA>        <NA>        <NA>                    Locklear 2011
990           <NA>        <NA>        <NA>                    Locklear 2011
991           <NA>        <NA>        <NA>                    Locklear 2011
992           <NA>        <NA>        <NA>                    Locklear 2011
993           <NA>        <NA>        <NA>                    Locklear 2011
994           <NA>        <NA>        <NA>                    Locklear 2011
995           <NA>        <NA>        <NA>                    Locklear 2011
996           <NA>        <NA>        <NA>                    Locklear 2011
997           <NA>        <NA>        <NA>                    Locklear 2011
998           <NA>        <NA>        <NA>                    Locklear 2011
999           <NA>        <NA>        <NA>                    Locklear 2011
1000          <NA>        <NA>        <NA>                    Locklear 2011
1001          <NA>        <NA>        <NA>                    Locklear 2011
1002          <NA>        <NA>        <NA>                Grant  Grant 1965
1003          <NA>        <NA>        <NA>                Grant  Grant 1965
1004          <NA>        <NA>        <NA>                Grant  Grant 1965
1005          <NA>        <NA>        <NA>                    Jepson Online
1006          <NA>        <NA>        <NA>                    Jepson Online
1007          <NA>        <NA>        <NA>                    Jepson Online
1008          <NA>        <NA>        <NA>                    Jepson Online
1009          <NA>        <NA>        <NA>                    Jepson Online
1010          <NA>        <NA>        <NA>                    Jepson Online
1011          <NA>        <NA>        <NA>                Grant  Grant 1965
1012          <NA>        <NA>        <NA>                Grant  Grant 1965
1013          <NA>        <NA>        <NA>                Grant  Grant 1965
1014          <NA>        <NA>        <NA>                Grant  Grant 1965
1015          <NA>        <NA>        <NA>                Grant  Grant 1965
1016          <NA>        <NA>        <NA>                Grant  Grant 1965
1017          <NA>        <NA>        <NA>                Grant  Grant 1965
1018          <NA>        <NA>        <NA>                Grant  Grant 1965
1019          <NA>        <NA>        <NA>                Grant  Grant 1965
1020          <NA>        <NA>        <NA>                Grant  Grant 1965
1021          <NA>        <NA>        <NA>                Grant  Grant 1965
1022          <NA>        <NA>        <NA>                Grant  Grant 1965
1023          <NA>        <NA>        <NA>                Grant  Grant 1965
1024          <NA>        <NA>        <NA>                Grant  Grant 1965
1025          <NA>        <NA>        <NA>                    Jepson Online
1026          <NA>        <NA>        <NA>                    Jepson Online
1027          <NA>        <NA>        <NA>                    Jepson Online
1028          <NA>        <NA>        <NA>                Grant  Grant 1965
1029          <NA>        <NA>        <NA>                Grant  Grant 1965
1030          <NA>        <NA>        <NA>                Grant  Grant 1965
1031          <NA>        <NA>        <NA>                Grant  Grant 1965
1032          <NA>        <NA>        <NA>                Grant  Grant 1965
1033          <NA>        <NA>        <NA>                Grant  Grant 1965
1034          <NA>        <NA>        <NA>                Grant  Grant 1965
1035          <NA>        <NA>        <NA>                Grant  Grant 1965
1036          <NA>        <NA>        <NA>                Grant  Grant 1965
1037          <NA>        <NA>        <NA>                Grant  Grant 1965
1038          <NA>        <NA>        <NA>                Grant  Grant 1965
1039          <NA>        <NA>        <NA>                Grant  Grant 1965
1040          <NA>        <NA>        <NA>                Grant  Grant 1965
1041          <NA>        <NA>        <NA>                Grant  Grant 1965
1042          <NA>        <NA>        <NA>                Grant  Grant 1965
1043          <NA>        <NA>        <NA>                Grant  Grant 1965
1044          <NA>        <NA>        <NA>                Grant  Grant 1965
1045          <NA>        <NA>        <NA>                    Jepson Online
1046          <NA>        <NA>        <NA>                    Jepson Online
1047          <NA>        <NA>        <NA>                    Jepson Online
1048          <NA>        <NA>        <NA>                    Jepson Online
1049          <NA>        <NA>        <NA>                Grant  Grant 1965
1050          <NA>        <NA>        <NA>                Grant  Grant 1965
1051          <NA>        <NA>        <NA>                Grant  Grant 1965
1052          <NA>        <NA>        <NA>                Grant  Grant 1965
1053          <NA>        <NA>        <NA>                Grant  Grant 1965
1054          <NA>        <NA>        <NA>                Grant  Grant 1965
1055     pale blue        <NA>        <NA>                    Locklear 2011
1056     pale blue        <NA>        <NA>                    Locklear 2011
1057     pale blue        <NA>        <NA>                    Locklear 2011
1058     pale blue        <NA>        <NA>                    Locklear 2011
1059     pale blue        <NA>        <NA>                    Locklear 2011
1060     pale blue        <NA>        <NA>                    Locklear 2011
1061     pale blue        <NA>        <NA>                    Locklear 2011
1062     pale blue        <NA>        <NA>                    Locklear 2011
1063     pale blue        <NA>        <NA>                    Locklear 2011
1064         white        <NA>        <NA>                    Locklear 2011
1065         white        <NA>        <NA>                    Locklear 2011
1066         white        <NA>        <NA>                    Locklear 2011
1067         white        <NA>        <NA>                    Locklear 2011
1068         white        <NA>        <NA>                    Locklear 2011
1069         white        <NA>        <NA>                    Locklear 2011
1070          <NA>        <NA>        <NA>                    Locklear 2011
1071          <NA>        <NA>        <NA>                    Locklear 2011
1072          <NA>        <NA>        <NA>                    Locklear 2011
1073          <NA>        <NA>        <NA>                    Locklear 2011
1074          <NA>        <NA>        <NA>                    Locklear 2011
1075          <NA>        <NA>        <NA>                    Locklear 2011
1076          <NA>        <NA>        <NA>                    Locklear 2011
1077          <NA>        <NA>        <NA>                    Locklear 2011
1078          <NA>        <NA>        <NA>                    Locklear 2011
1079          <NA>        <NA>        <NA>                    Locklear 2011
1080          <NA>        <NA>        <NA>                    Locklear 2011
1081          <NA>        <NA>        <NA>                    Locklear 2011
1082          <NA>        <NA>        <NA>                    Locklear 2011
1083          <NA>        <NA>        <NA>                    Locklear 2011
1084          <NA>        <NA>        <NA>                    Locklear 2011
1085          pink       white        <NA>                    Locklear 2011
1086          pink       white        <NA>                    Locklear 2011
1087          pink       white        <NA>                    Locklear 2011
1088          pink       white        <NA>                    Locklear 2011
1089          pink       white        <NA>                    Locklear 2011
1090          pink       white        <NA>                    Locklear 2011
1091          pink       white        <NA>                    Locklear 2011
1092          pink       white        <NA>                    Locklear 2011
1093          pink       white        <NA>                    Locklear 2011
1094         white        <NA>        <NA>                    Locklear 2011
1095         white        <NA>        <NA>                    Locklear 2011
1096         white        <NA>        <NA>                    Locklear 2011
1097         white        <NA>        <NA>                    Locklear 2011
1098         white        <NA>        <NA>                    Locklear 2011
1099         white        <NA>        <NA>                    Locklear 2011
1100         white        <NA>        <NA>                    Locklear 2011
1101         white        <NA>        <NA>                    Locklear 2011
1102         white        <NA>        <NA>                    Locklear 2011
1103         white        <NA>        <NA>                    Locklear 2011
1104         white        <NA>        <NA>                    Locklear 2011
1105         white        <NA>        <NA>                    Locklear 2011
1106         white        <NA>        <NA>                    Locklear 2011
1107         white        <NA>        <NA>                    Locklear 2011
1108         white        <NA>        <NA>                    Locklear 2011
1109         white        <NA>        <NA>                    Locklear 2011
1110         white        <NA>        <NA>                    Locklear 2011
1111         white        <NA>        <NA>                    Locklear 2011
1112         white        <NA>        <NA>                    Locklear 2011
1113         white        <NA>        <NA>                    Locklear 2011
1114         white        <NA>        <NA>                    Locklear 2011
1115         white        <NA>        <NA>                    Locklear 2011
1116         white        <NA>        <NA>                    Locklear 2011
1117          pink       white        <NA>                    Locklear 2011
1118          pink       white        <NA>                    Locklear 2011
1119          pink       white        <NA>                    Locklear 2011
1120          pink       white        <NA>                    Locklear 2011
1121          pink       white        <NA>                    Locklear 2011
1122          pink       white        <NA>                    Locklear 2011
1123          pink       white        <NA>                    Locklear 2011
1124          pink       white        <NA>                    Locklear 2011
1125          pink       white        <NA>                    Locklear 2011
1126          pink       white        <NA>                    Locklear 2011
1127          pink       white        <NA>                    Locklear 2011
1128          pink       white        <NA>                    Locklear 2011
1129          pink       white        <NA>                    Locklear 2011
1130          pink       white        <NA>                    Locklear 2011
1131          pink       white        <NA>                    Locklear 2011
1132          pink       white        <NA>                    Locklear 2011
1133          pink       white        <NA>                    Locklear 2011
1134          pink       white        <NA>                    Locklear 2011
1135          pink       white        <NA>                    Locklear 2011
1136          pink       white        <NA>                    Locklear 2011
1137          pink       white        <NA>                    Locklear 2011
1138          pink       white        <NA>                    Locklear 2011
1139          pink       white        <NA>                    Locklear 2011
1140          pink       white        <NA>                    Locklear 2011
1141          pink       white        <NA>                    Locklear 2011
1142          pink       white        <NA>                    Locklear 2011
1143          pink       white        <NA>                    Locklear 2011
1144          pink       white        <NA>                    Locklear 2011
1145          pink       white        <NA>                    Locklear 2011
1146          pink       white        <NA>                    Locklear 2011
1147          pink       white        <NA>                    Locklear 2011
1148          pink       white        <NA>                    Locklear 2011
1149          pink       white        <NA>                    Locklear 2011
1150          pink       white        <NA>                    Locklear 2011
1151          pink       white        <NA>                    Locklear 2011
1152          pink       white        <NA>                    Locklear 2011
1153          pink       white        <NA>                    Locklear 2011
1154          pink       white        <NA>                    Locklear 2011
1155          pink       white        <NA>                    Locklear 2011
1156          pink       white        <NA>                    Locklear 2011
1157          pink       white        <NA>                    Locklear 2011
1158          pink       white        <NA>                    Locklear 2011
1159          pink       white        <NA>                    Locklear 2011
1160          pink       white        <NA>                    Locklear 2011
1161          pink       white        <NA>                    Locklear 2011
1162          pink       white        <NA>                    Locklear 2011
1163          pink       white        <NA>                    Locklear 2011
1164          pink       white        <NA>                    Locklear 2011
1165          pink       white        <NA>                    Locklear 2011
1166          pink       white        <NA>                    Locklear 2011
1167          pink       white        <NA>                    Locklear 2011
1168          pink       white        <NA>                    Locklear 2011
1169          pink       white        <NA>                    Locklear 2011
1170          pink       white        <NA>                    Locklear 2011
1171          pink       white        <NA>                    Locklear 2011
1172          pink       white        <NA>                    Locklear 2011
1173          pink       white        <NA>                    Locklear 2011
1174          pink       white        <NA>                    Locklear 2011
1175          pink       white        <NA>                    Locklear 2011
1176          pink       white        <NA>                    Locklear 2011
1177          pink       white        <NA>                    Locklear 2011
1178          pink       white        <NA>                    Locklear 2011
1179          pink       white        <NA>                    Locklear 2011
1180          pink       white        <NA>                    Locklear 2011
1181          pink       white        <NA>                    Locklear 2011
1182          pink       white        <NA>                    Locklear 2011
1183          pink       white        <NA>                    Locklear 2011
1184          pink       white        <NA>                    Locklear 2011
1185          pink       white        <NA>                    Locklear 2011
1186          pink       white        <NA>                    Locklear 2011
1187          pink       white        <NA>                    Locklear 2011
1188          pink       white        <NA>                    Locklear 2011
1189          pink       white        <NA>                    Locklear 2011
1190          pink       white        <NA>                    Locklear 2011
1191          pink       white        <NA>                    Locklear 2011
1192          pink       white        <NA>                    Locklear 2011
1193          pink       white        <NA>                    Locklear 2011
1194          pink       white        <NA>                    Locklear 2011
1195          pink       white        <NA>                    Locklear 2011
1196          <NA>        <NA>        <NA>                    Locklear 2011
1197          <NA>        <NA>        <NA>                    Locklear 2011
1198          <NA>        <NA>        <NA>                    Locklear 2011
1199          <NA>        <NA>        <NA>                    Locklear 2011
1200          <NA>        <NA>        <NA>                    Locklear 2011
1201          <NA>        <NA>        <NA>                    Locklear 2011
1202          <NA>        <NA>        <NA>                    Locklear 2011
1203          <NA>        <NA>        <NA>                    Locklear 2011
1204          <NA>        <NA>        <NA>                    Locklear 2011
1205          pink       white        <NA>                    Locklear 2011
1206          pink       white        <NA>                    Locklear 2011
1207          pink       white        <NA>                    Locklear 2011
1208          pink       white        <NA>                    Locklear 2011
1209          pink       white        <NA>                    Locklear 2011
1210          pink       white        <NA>                    Locklear 2011
1211          <NA>        <NA>        <NA>                    Locklear 2011
1212          <NA>        <NA>        <NA>                    Locklear 2011
1213          <NA>        <NA>        <NA>                    Locklear 2011
1214          <NA>        <NA>        <NA>                    Locklear 2011
1215          <NA>        <NA>        <NA>                    Locklear 2011
1216          <NA>        <NA>        <NA>                    Locklear 2011
1217          <NA>        <NA>        <NA>                    Locklear 2011
1218          <NA>        <NA>        <NA>                    Locklear 2011
1219          <NA>        <NA>        <NA>                    Locklear 2011
1220          <NA>        <NA>        <NA>                    Locklear 2011
1221          <NA>        <NA>        <NA>                    Locklear 2011
1222          <NA>        <NA>        <NA>                    Locklear 2011
1223          pink       white        <NA>                    Locklear 2011
1224          pink       white        <NA>                    Locklear 2011
1225          pink       white        <NA>                    Locklear 2011
1226          pink       white        <NA>                    Locklear 2011
1227          pink       white        <NA>                    Locklear 2011
1228          pink       white        <NA>                    Locklear 2011
1229          pink       white        <NA>                    Locklear 2011
1230          pink       white        <NA>                    Locklear 2011
1231          pink       white        <NA>                    Locklear 2011
1232          pink       white        <NA>                    Locklear 2011
1233          pink       white        <NA>                    Locklear 2011
1234          pink       white        <NA>                    Locklear 2011
1235          pink       white        <NA>                    Locklear 2011
1236          pink       white        <NA>                    Locklear 2011
1237          pink       white        <NA>                    Locklear 2011
1238          pink       white        <NA>                    Locklear 2011
1239          pink       white        <NA>                    Locklear 2011
1240          pink       white        <NA>                    Locklear 2011
1241          pink       white        <NA>                    Locklear 2011
1242          <NA>        <NA>        <NA>                Grant  Grant 1965
1243          <NA>        <NA>        <NA>                Grant  Grant 1965
1244          <NA>        <NA>        <NA>                Grant  Grant 1965
1245          <NA>        <NA>        <NA>                Grant  Grant 1965
1246          <NA>        <NA>        <NA>                Grant  Grant 1965
1247          <NA>        <NA>        <NA>                    Jepson Online
1248          <NA>        <NA>        <NA>                    Jepson Online
1249          <NA>        <NA>        <NA>                    Jepson Online
1250          <NA>        <NA>        <NA>                    Jepson Online
1251          <NA>        <NA>        <NA>                    Jepson Online
1252          <NA>        <NA>        <NA>                    Jepson Online
1253          <NA>        <NA>        <NA>                    Jepson Online
1254          <NA>        <NA>        <NA>                    Jepson Online
1255          <NA>        <NA>        <NA>                    Jepson Online
1256          <NA>        <NA>        <NA>                Grant  Grant 1965
1257          <NA>        <NA>        <NA>                Grant  Grant 1965
1258          <NA>        <NA>        <NA>                Grant  Grant 1965
1259          <NA>        <NA>        <NA>                Grant  Grant 1965
1260          <NA>        <NA>        <NA>                Grant  Grant 1965
1261          <NA>        <NA>        <NA>                Grant  Grant 1965
1262          <NA>        <NA>        <NA>                Grant  Grant 1965
1263          <NA>        <NA>        <NA>                Grant  Grant 1965
1264          <NA>        <NA>        <NA>                Grant  Grant 1965
1265          <NA>        <NA>        <NA>                Grant  Grant 1965
1266          <NA>        <NA>        <NA>                Grant  Grant 1965
1267          <NA>        <NA>        <NA>                Grant  Grant 1965
1268          <NA>        <NA>        <NA>                Grant  Grant 1965
1269          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1270          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1271          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1272          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1273          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1274          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1275          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1276          <NA>        <NA>        <NA> Univ. of Wyoming Digital Archive
1277          <NA>        <NA>        <NA>                Grant  Grant 1965
1278          <NA>        <NA>        <NA>                Grant  Grant 1965
1279          <NA>        <NA>        <NA>                Grant  Grant 1965
1280          <NA>        <NA>        <NA>                Grant  Grant 1965
1281          <NA>        <NA>        <NA>                Grant  Grant 1965
1282          <NA>        <NA>        <NA>                Grant  Grant 1965
1283          <NA>        <NA>        <NA>                Grant  Grant 1965
1284          <NA>        <NA>        <NA>                Grant  Grant 1965
1285          <NA>        <NA>        <NA>                Grant  Grant 1965
1286          <NA>        <NA>        <NA>                Grant  Grant 1965
1287          <NA>        <NA>        <NA>                Grant  Grant 1965
1288          <NA>        <NA>        <NA>                Grant  Grant 1965
1289         white        <NA>        <NA>                Grant  Grant 1965
1290         white        <NA>        <NA>                Grant  Grant 1965
1291         white        <NA>        <NA>                Grant  Grant 1965
1292         white        <NA>        <NA>                Grant  Grant 1965
1293         white        <NA>        <NA>                Grant  Grant 1965
1294         white        <NA>        <NA>                Grant  Grant 1965
1295         white        <NA>        <NA>                Grant  Grant 1965
1296         white        <NA>        <NA>                Grant  Grant 1965
1297         white        <NA>        <NA>                Grant  Grant 1965
1298         white        <NA>        <NA>                Grant  Grant 1965
1299         white        <NA>        <NA>                Grant  Grant 1965
1300         white        <NA>        <NA>                Grant  Grant 1965
1301         white        <NA>        <NA>                Grant  Grant 1965
1302         white        <NA>        <NA>                Grant  Grant 1965
1303         white        <NA>        <NA>                Grant  Grant 1965
1304         white        <NA>        <NA>                Grant  Grant 1965
1305         white        <NA>        <NA>                Grant  Grant 1965
1306         white        <NA>        <NA>                Grant  Grant 1965
1307         white        <NA>        <NA>                Grant  Grant 1965
1308         white        <NA>        <NA>                Grant  Grant 1965
1309          <NA>        <NA>        <NA>                Grant  Grant 1965
1310          <NA>        <NA>        <NA>                Grant  Grant 1965
1311          <NA>        <NA>        <NA>                Grant  Grant 1965
1312          <NA>        <NA>        <NA>                Grant  Grant 1965
1313          <NA>        <NA>        <NA>                Grant  Grant 1965
1314          <NA>        <NA>        <NA>                Grant  Grant 1965
1315          <NA>        <NA>        <NA>                Grant  Grant 1965
1316          <NA>        <NA>        <NA>                Grant  Grant 1965
1317          <NA>        <NA>        <NA>                Grant  Grant 1965
1318          <NA>        <NA>        <NA>                Grant  Grant 1965
1319          <NA>        <NA>        <NA>                Grant  Grant 1965
1320          <NA>        <NA>        <NA>                Grant  Grant 1965
1321          <NA>        <NA>        <NA>                Grant  Grant 1965
1322          <NA>        <NA>        <NA>                Grant  Grant 1965
1323          <NA>        <NA>        <NA>                Grant  Grant 1965
1324          <NA>        <NA>        <NA>                Grant  Grant 1965
1325          <NA>        <NA>        <NA>                Grant  Grant 1965
1326          <NA>        <NA>        <NA>                Grant  Grant 1965
1327          <NA>        <NA>        <NA>                Grant  Grant 1965
1328          <NA>        <NA>        <NA>                Grant  Grant 1965
1329          <NA>        <NA>        <NA>                Grant  Grant 1965
1330          <NA>        <NA>        <NA>                Grant  Grant 1965
1331          <NA>        <NA>        <NA>                Grant  Grant 1965
1332          <NA>        <NA>        <NA>                Grant  Grant 1965
1333          <NA>        <NA>        <NA>                Grant  Grant 1965
1334          <NA>        <NA>        <NA>                Grant  Grant 1965
1335          <NA>        <NA>        <NA>                Grant  Grant 1965
1336          <NA>        <NA>        <NA>                Grant  Grant 1965
1337          <NA>        <NA>        <NA>                Grant  Grant 1965
1338          <NA>        <NA>        <NA>                Grant  Grant 1965
1339          <NA>        <NA>        <NA>                Grant  Grant 1965
1340          <NA>        <NA>        <NA>                Grant  Grant 1965
1341          <NA>        <NA>        <NA>                Grant  Grant 1965
1342          <NA>        <NA>        <NA>                Grant  Grant 1965
1343          <NA>        <NA>        <NA>                Grant  Grant 1965
1344          <NA>        <NA>        <NA>                Grant  Grant 1965
1345          <NA>        <NA>        <NA>                Grant  Grant 1965
1346          <NA>        <NA>        <NA>                Grant  Grant 1965
1347          <NA>        <NA>        <NA>                Grant  Grant 1965
1348          <NA>        <NA>        <NA>                Grant  Grant 1965
1349          <NA>        <NA>        <NA>                Grant  Grant 1965
1350          <NA>        <NA>        <NA>        Flora of British Columbia
1351          <NA>        <NA>        <NA>        Flora of British Columbia
1352          <NA>        <NA>        <NA>        Flora of British Columbia
1353          <NA>        <NA>        <NA>        Flora of British Columbia
1354          <NA>        <NA>        <NA>        Flora of British Columbia
1355          <NA>        <NA>        <NA>        Flora of British Columbia
1356          <NA>        <NA>        <NA>        Flora of British Columbia
1357          <NA>        <NA>        <NA>        Flora of British Columbia
1358          <NA>        <NA>        <NA>        Flora of British Columbia
1359          <NA>        <NA>        <NA>        Flora of British Columbia
1360          <NA>        <NA>        <NA>                    Jepson Online
1361          <NA>        <NA>        <NA>                    Jepson Online
1362          <NA>        <NA>        <NA>                    Jepson Online
1363          <NA>        <NA>        <NA>                    Jepson Online
1364          <NA>        <NA>        <NA>                    Jepson Online
1365          <NA>        <NA>        <NA>                    Jepson Online
1366          <NA>        <NA>        <NA>                    Jepson Online
1367          <NA>        <NA>        <NA>                    Jepson Online
1368          <NA>        <NA>        <NA>                    Jepson Online
1369          <NA>        <NA>        <NA>                    Jepson Online
1370          <NA>        <NA>        <NA>                    Jepson Online
1371          <NA>        <NA>        <NA>                    Jepson Online
1372          <NA>        <NA>        <NA>                    Jepson Online
1373          <NA>        <NA>        <NA>                    Jepson Online
1374          <NA>        <NA>        <NA>                    Jepson Online
1375          <NA>        <NA>        <NA>                    Jepson Online
1376          <NA>        <NA>        <NA>                    Jepson Online
1377          <NA>        <NA>        <NA>                    Jepson Online
1378          <NA>        <NA>        <NA>                    Jepson Online
1379          <NA>        <NA>        <NA>                    Jepson Online
1380          <NA>        <NA>        <NA>                    Jepson Online
1381          <NA>        <NA>        <NA>                    Jepson Online
1382          <NA>        <NA>        <NA>                    Jepson Online
1383          <NA>        <NA>        <NA>                    Jepson Online
     pollinator_1  pollinator_2 pollinator_3               pollinator_source
1     hummingbird          <NA>         <NA>                 Hsu,  Hall 2003
2     hummingbird          <NA>         <NA>                 Hsu,  Hall 2003
3     hummingbird          <NA>         <NA>                 Hsu,  Hall 2003
4     hummingbird          <NA>         <NA>                 Hsu,  Hall 2003
5     hummingbird          <NA>         <NA>              Grant,  Grant 1965
6     hummingbird          <NA>         <NA>              Grant,  Grant 1965
7     hummingbird          <NA>         <NA>              Grant,  Grant 1965
8            <NA>          <NA>         <NA>              Grant,  Grant 1965
9            <NA>          <NA>         <NA>              Grant,  Grant 1965
10           <NA>          <NA>         <NA>              Grant,  Grant 1965
11           <NA>          <NA>         <NA>              Grant,  Grant 1965
12           <NA>          <NA>         <NA>              Grant,  Grant 1965
13           <NA>          <NA>         <NA>              Grant,  Grant 1965
14           <NA>          <NA>         <NA>              Grant,  Grant 1965
15           <NA>          <NA>         <NA>              Grant,  Grant 1965
16           <NA>          <NA>         <NA>              Grant,  Grant 1965
17           <NA>          <NA>         <NA>              Grant,  Grant 1965
18           <NA>          <NA>         <NA>              Grant,  Grant 1965
19           <NA>          <NA>         <NA>              Grant,  Grant 1965
20           <NA>          <NA>         <NA>              Grant,  Grant 1965
21           <NA>          <NA>         <NA>              Grant,  Grant 1965
22           <NA>          <NA>         <NA>              Grant,  Grant 1965
23           <NA>          <NA>         <NA>              Grant,  Grant 1965
24           <NA>          <NA>         <NA>              Grant,  Grant 1965
25           <NA>          <NA>         <NA>              Grant,  Grant 1965
26            bee          <NA>         <NA>              Grant,  Grant 1965
27            bee          <NA>         <NA>              Grant,  Grant 1965
28            bee          <NA>         <NA>              Grant,  Grant 1965
29            bee          <NA>         <NA>              Grant,  Grant 1965
30            bee          <NA>         <NA>              Grant,  Grant 1965
31            bee          <NA>         <NA>              Grant,  Grant 1965
32    hummingbird          <NA>         <NA>              Grant,  Grant 1965
33    hummingbird          <NA>         <NA>              Grant,  Grant 1965
34    hummingbird          <NA>         <NA>              Grant,  Grant 1965
35    hummingbird          <NA>         <NA>              Grant,  Grant 1965
36    hummingbird          <NA>         <NA>              Grant,  Grant 1965
37    hummingbird          <NA>         <NA>              Grant,  Grant 1965
38    hummingbird          <NA>         <NA>              Grant,  Grant 1965
39    hummingbird          <NA>         <NA>              Grant,  Grant 1965
40    hummingbird          <NA>         <NA>              Grant,  Grant 1965
41    hummingbird          <NA>         <NA>              Grant,  Grant 1965
42    hummingbird          <NA>         <NA>              Grant,  Grant 1965
43    hummingbird          <NA>         <NA>              Grant,  Grant 1965
44    hummingbird          <NA>         <NA>              Grant,  Grant 1965
45    hummingbird          <NA>         <NA>              Grant,  Grant 1965
46    hummingbird          <NA>         <NA>              Grant,  Grant 1965
47    hummingbird          <NA>         <NA>              Grant,  Grant 1965
48    hummingbird          <NA>         <NA>              Grant,  Grant 1965
49    hummingbird          <NA>         <NA>              Grant,  Grant 1965
50        bee-fly          <NA>         <NA>              Grant,  Grant 1965
51        bee-fly          <NA>         <NA>              Grant,  Grant 1965
52        bee-fly          <NA>         <NA>              Grant,  Grant 1965
53           <NA>          <NA>         <NA>              Grant,  Grant 1965
54           <NA>          <NA>         <NA>              Grant,  Grant 1965
55           <NA>          <NA>         <NA>              Grant,  Grant 1965
56           <NA>          <NA>         <NA>              Grant,  Grant 1965
57           <NA>          <NA>         <NA>              Grant,  Grant 1965
58           <NA>          <NA>         <NA>              Grant,  Grant 1965
59           <NA>          <NA>         <NA>              Grant,  Grant 1965
60           <NA>          <NA>         <NA>              Grant,  Grant 1965
61           <NA>          <NA>         <NA>              Grant,  Grant 1965
62           <NA>          <NA>         <NA>              Grant,  Grant 1965
63           <NA>          <NA>         <NA>              Grant,  Grant 1965
64           <NA>          <NA>         <NA>              Grant,  Grant 1965
65           <NA>          <NA>         <NA>              Grant,  Grant 1965
66           <NA>          <NA>         <NA>              Grant,  Grant 1965
67           <NA>          <NA>         <NA>              Grant,  Grant 1965
68           <NA>          <NA>         <NA>              Grant,  Grant 1965
69           <NA>          <NA>         <NA>              Grant,  Grant 1965
70           <NA>          <NA>         <NA>              Grant,  Grant 1965
71           <NA>          <NA>         <NA>              Grant,  Grant 1965
72           <NA>          <NA>         <NA>              Grant,  Grant 1965
73           <NA>          <NA>         <NA>              Grant,  Grant 1965
74           <NA>          <NA>         <NA>              Grant,  Grant 1965
75           <NA>          <NA>         <NA>              Grant,  Grant 1965
76           <NA>          <NA>         <NA>              Grant,  Grant 1965
77           <NA>          <NA>         <NA>              Grant,  Grant 1965
78           <NA>          <NA>         <NA>              Grant,  Grant 1965
79           <NA>          <NA>         <NA>              Grant,  Grant 1965
80           <NA>          <NA>         <NA>              Grant,  Grant 1965
81           <NA>          <NA>         <NA>              Grant,  Grant 1965
82           <NA>          <NA>         <NA>              Grant,  Grant 1965
83            bee       bee-fly         <NA>              Grant,  Grant 1965
84            bee       bee-fly         <NA>              Grant,  Grant 1965
85            bee       bee-fly         <NA>              Grant,  Grant 1965
86            bee       bee-fly         <NA>              Grant,  Grant 1965
87            bee       bee-fly         <NA>              Grant,  Grant 1965
88            bee       bee-fly         <NA>              Grant,  Grant 1965
89           <NA>          <NA>         <NA>              Grant,  Grant 1965
90           <NA>          <NA>         <NA>              Grant,  Grant 1965
91           <NA>          <NA>         <NA>              Grant,  Grant 1965
92           <NA>          <NA>         <NA>              Grant,  Grant 1965
93           <NA>          <NA>         <NA>              Grant,  Grant 1965
94           <NA>          <NA>         <NA>              Grant,  Grant 1965
95           <NA>          <NA>         <NA>              Grant,  Grant 1965
96           <NA>          <NA>         <NA>              Grant,  Grant 1965
97           <NA>          <NA>         <NA>              Grant,  Grant 1965
98           <NA>          <NA>         <NA>              Grant,  Grant 1965
99           <NA>          <NA>         <NA>              Grant,  Grant 1965
100          <NA>          <NA>         <NA>              Grant,  Grant 1965
101          <NA>          <NA>         <NA>              Grant,  Grant 1965
102          <NA>          <NA>         <NA>              Grant,  Grant 1965
103          <NA>          <NA>         <NA>              Grant,  Grant 1965
104          <NA>          <NA>         <NA>              Grant,  Grant 1965
105          <NA>          <NA>         <NA>              Grant,  Grant 1965
106          <NA>          <NA>         <NA>              Grant,  Grant 1965
107           bee          <NA>         <NA>              Grant,  Grant 1965
108           bee          <NA>         <NA>              Grant,  Grant 1965
109           bee          <NA>         <NA>              Grant,  Grant 1965
110           bee          <NA>         <NA>              Grant,  Grant 1965
111           bee          <NA>         <NA>              Grant,  Grant 1965
112           bee          <NA>         <NA>              Grant,  Grant 1965
113   hummingbird          <NA>         <NA>              Grant,  Grant 1965
114   hummingbird          <NA>         <NA>              Grant,  Grant 1965
115   hummingbird          <NA>         <NA>              Grant,  Grant 1965
116   hummingbird          <NA>         <NA>              Grant,  Grant 1965
117   hummingbird          <NA>         <NA>              Grant,  Grant 1965
118   hummingbird          <NA>         <NA>              Grant,  Grant 1965
119   hummingbird          <NA>         <NA>              Grant,  Grant 1965
120   hummingbird          <NA>         <NA>              Grant,  Grant 1965
121   hummingbird          <NA>         <NA>              Grant,  Grant 1965
122   hummingbird          <NA>         <NA>              Grant,  Grant 1965
123   hummingbird          <NA>         <NA>              Grant,  Grant 1965
124   hummingbird          <NA>         <NA>              Grant,  Grant 1965
125   hummingbird          <NA>         <NA>              Grant,  Grant 1965
126   hummingbird          <NA>         <NA>              Grant,  Grant 1965
127   hummingbird          <NA>         <NA>              Grant,  Grant 1965
128   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
129   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
130   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
131   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
132   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
133   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
134   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
135   hummingbird      hawkmoth         <NA>              Grant,  Grant 1965
136           bat          <NA>         <NA>                   Prather 1999b
137           bat          <NA>         <NA>                   Prather 1999b
138   hummingbird          <NA>         <NA>                   Prather 1999b
139          <NA>          <NA>         <NA>                   Prather 1999b
140          <NA>          <NA>         <NA>                   Prather 1999b
141          <NA>          <NA>         <NA>                   Prather 1999b
142          <NA>          <NA>         <NA>                   Prather 1999b
143          <NA>          <NA>         <NA>                   Prather 1999b
144          <NA>          <NA>         <NA>                   Prather 1999b
145          <NA>          <NA>         <NA>                   Prather 1999b
146          <NA>          <NA>         <NA>                   Prather 1999b
147          <NA>          <NA>         <NA>                   Prather 1999b
148          <NA>          <NA>         <NA>                   Prather 1999b
149          <NA>          <NA>         <NA>                   Prather 1999b
150   hummingbird          <NA>         <NA>                   Prather 1999b
151   hummingbird          <NA>         <NA>                   Prather 1999b
152   hummingbird          <NA>         <NA>                   Prather 1999b
153      hawkmoth          <NA>         <NA>                   Prather 1999b
154      hawkmoth          <NA>         <NA>                   Prather 1999b
155          <NA>          <NA>         <NA>                   Prather 1999b
156          <NA>          <NA>         <NA>                   Prather 1999b
157           bat          <NA>         <NA>                   Prather 1999b
158           bat          <NA>         <NA>                   Prather 1999b
159           bat          <NA>         <NA>                   Prather 1999b
160           bat          <NA>         <NA>                   Prather 1999b
161           bat          <NA>         <NA>                   Prather 1999b
162           bat          <NA>         <NA>                   Prather 1999b
163           bat          <NA>         <NA>                   Prather 1999b
164           bat          <NA>         <NA>                   Prather 1999b
165           bee          <NA>         <NA>              Grant,  Grant 1965
166           bee          <NA>         <NA>              Grant,  Grant 1965
167           bee          <NA>         <NA>              Grant,  Grant 1965
168           bee          <NA>         <NA>              Grant,  Grant 1965
169           bee          <NA>         <NA>              Grant,  Grant 1965
170           bee          <NA>         <NA>              Grant,  Grant 1965
171           bee          <NA>         <NA>              Grant,  Grant 1965
172           bee          <NA>         <NA>              Grant,  Grant 1965
173           bee          <NA>         <NA>              Grant,  Grant 1965
174           bee          <NA>         <NA>              Grant,  Grant 1965
175           bee          <NA>         <NA>              Grant,  Grant 1965
176           bee          <NA>         <NA>              Grant,  Grant 1965
177           bee          <NA>         <NA>              Grant,  Grant 1965
178           bee          <NA>         <NA>              Grant,  Grant 1965
179           bee          <NA>         <NA>              Grant,  Grant 1965
180           bee          <NA>         <NA>              Grant,  Grant 1965
181           bee          <NA>         <NA>              Grant,  Grant 1965
182           bee          <NA>         <NA>              Grant,  Grant 1965
183           bee          <NA>         <NA>              Grant,  Grant 1965
184           bee          <NA>         <NA>              Grant,  Grant 1965
185           bee          <NA>         <NA>              Grant,  Grant 1965
186           bee          <NA>         <NA>              Grant,  Grant 1965
187           bee          <NA>         <NA>              Grant,  Grant 1965
188           bee          <NA>         <NA>              Grant,  Grant 1965
189           bee          <NA>         <NA>              Grant,  Grant 1965
190          <NA>          <NA>         <NA>              Grant,  Grant 1965
191          <NA>          <NA>         <NA>              Grant,  Grant 1965
192          <NA>          <NA>         <NA>              Grant,  Grant 1965
193          <NA>          <NA>         <NA>              Grant,  Grant 1965
194          <NA>          <NA>         <NA>              Grant,  Grant 1965
195          <NA>          <NA>         <NA>              Grant,  Grant 1965
196          <NA>          <NA>         <NA>              Grant,  Grant 1965
197          <NA>          <NA>         <NA>              Grant,  Grant 1965
198          <NA>          <NA>         <NA>              Grant,  Grant 1965
199          <NA>          <NA>         <NA>              Grant,  Grant 1965
200          <NA>          <NA>         <NA>              Grant,  Grant 1965
201          <NA>          <NA>         <NA>              Grant,  Grant 1965
202          <NA>          <NA>         <NA>              Grant,  Grant 1965
203          <NA>          <NA>         <NA>              Grant,  Grant 1965
204          <NA>          <NA>         <NA>              Grant,  Grant 1965
205          <NA>          <NA>         <NA>              Grant,  Grant 1965
206          <NA>          <NA>         <NA>              Grant,  Grant 1965
207          <NA>          <NA>         <NA>              Grant,  Grant 1965
208          <NA>          <NA>         <NA>              Grant,  Grant 1965
209          <NA>          <NA>         <NA>              Grant,  Grant 1965
210          <NA>          <NA>         <NA>              Grant,  Grant 1965
211          <NA>          <NA>         <NA>              Grant,  Grant 1965
212          <NA>          <NA>         <NA>              Grant,  Grant 1965
213          <NA>          <NA>         <NA>              Grant,  Grant 1965
214          <NA>          <NA>         <NA>              Grant,  Grant 1965
215          <NA>          <NA>         <NA>              Grant,  Grant 1965
216          <NA>          <NA>         <NA>              Grant,  Grant 1965
217          <NA>          <NA>         <NA>              Grant,  Grant 1965
218          <NA>          <NA>         <NA>              Grant,  Grant 1965
219          <NA>          <NA>         <NA>              Grant,  Grant 1965
220          <NA>          <NA>         <NA>              Grant,  Grant 1965
221          <NA>          <NA>         <NA>              Grant,  Grant 1965
222          <NA>          <NA>         <NA>              Grant,  Grant 1965
223          <NA>          <NA>         <NA>              Grant,  Grant 1965
224          <NA>          <NA>         <NA>              Grant,  Grant 1965
225          <NA>          <NA>         <NA>              Grant,  Grant 1965
226          <NA>          <NA>         <NA>              Grant,  Grant 1965
227          <NA>          <NA>         <NA>              Grant,  Grant 1965
228          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
229          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
230          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
231          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
232          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
233          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
234          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
235          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
236          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
237          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
238          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
239          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
240          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
241          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
242          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
243          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
244          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
245          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
246          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
247          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
248          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
249          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
250          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
251          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
252          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
253          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
254          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
255          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
256          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
257          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
258          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
259          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
260          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
261          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
262          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
263          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
264          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
265          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
266          <NA>          <NA>         <NA>  Joyal 1986; Grant,  Grant 1965
267       bee-fly          <NA>         <NA>              Grant,  Grant 1965
268       bee-fly          <NA>         <NA>              Grant,  Grant 1965
269       bee-fly          <NA>         <NA>              Grant,  Grant 1965
270       bee-fly          <NA>         <NA>              Grant,  Grant 1965
271       bee-fly          <NA>         <NA>              Grant,  Grant 1965
272       bee-fly          <NA>         <NA>              Grant,  Grant 1965
273   hummingbird          <NA>         <NA>              Grant,  Grant 1965
274   hummingbird          <NA>         <NA>              Grant,  Grant 1965
275   hummingbird          <NA>         <NA>              Grant,  Grant 1965
276   hummingbird          <NA>         <NA>              Grant,  Grant 1965
277   hummingbird          <NA>         <NA>              Grant,  Grant 1965
278   hummingbird          <NA>         <NA>              Grant,  Grant 1965
279   hummingbird          <NA>         <NA>              Grant,  Grant 1965
280   hummingbird          <NA>         <NA>              Grant,  Grant 1965
281   hummingbird          <NA>         <NA>              Grant,  Grant 1965
282          <NA>          <NA>         <NA>                      Joyal 1986
283          <NA>          <NA>         <NA>                      Joyal 1986
284          <NA>          <NA>         <NA>                      Joyal 1986
285          <NA>          <NA>         <NA>                      Joyal 1986
286          <NA>          <NA>         <NA>                      Joyal 1986
287          <NA>          <NA>         <NA>                      Joyal 1986
288          <NA>          <NA>         <NA>                      Joyal 1986
289          <NA>          <NA>         <NA>                      Joyal 1986
290          <NA>          <NA>         <NA>                      Joyal 1986
291     butterfly       bee-fly          bee Rancho Santa Ana Botanic Garden
292     butterfly       bee-fly          bee Rancho Santa Ana Botanic Garden
293           bee       bee-fly         <NA>              Grant,  Grant 1965
294           bee       bee-fly         <NA>              Grant,  Grant 1965
295           bee       bee-fly         <NA>              Grant,  Grant 1965
296           bee       bee-fly         <NA>              Grant,  Grant 1965
297           bee       bee-fly         <NA>              Grant,  Grant 1965
298           bee       bee-fly         <NA>              Grant,  Grant 1965
299           bee       bee-fly         <NA>              Grant,  Grant 1965
300           bee       bee-fly         <NA>              Grant,  Grant 1965
301           bee       bee-fly         <NA>              Grant,  Grant 1965
302           bee       bee-fly         <NA>              Grant,  Grant 1965
303           bee       bee-fly         <NA>              Grant,  Grant 1965
304           bee       bee-fly         <NA>              Grant,  Grant 1965
305           bee       bee-fly         <NA>              Grant,  Grant 1965
306           bee       bee-fly         <NA>              Grant,  Grant 1965
307           bee       bee-fly         <NA>              Grant,  Grant 1965
308           bee       bee-fly         <NA>              Grant,  Grant 1965
309           bee       bee-fly         <NA>              Grant,  Grant 1965
310           bee       bee-fly         <NA>              Grant,  Grant 1965
311           bee       bee-fly         <NA>              Grant,  Grant 1965
312           bee       bee-fly         <NA>              Grant,  Grant 1965
313           bee       bee-fly         <NA>              Grant,  Grant 1965
314       bee-fly          <NA>         <NA>              Grant,  Grant 1965
315       bee-fly          <NA>         <NA>              Grant,  Grant 1965
316       bee-fly          <NA>         <NA>              Grant,  Grant 1965
317       bee-fly          <NA>         <NA>              Grant,  Grant 1965
318       bee-fly          <NA>         <NA>              Grant,  Grant 1965
319       bee-fly          <NA>         <NA>              Grant,  Grant 1965
320       bee-fly          <NA>         <NA>              Grant,  Grant 1965
321       bee-fly          <NA>         <NA>              Grant,  Grant 1965
322       bee-fly          <NA>         <NA>              Grant,  Grant 1965
323       bee-fly          <NA>         <NA>              Grant,  Grant 1965
324       bee-fly          <NA>         <NA>              Grant,  Grant 1965
325       bee-fly          <NA>         <NA>              Grant,  Grant 1965
326       bee-fly          <NA>         <NA>              Grant,  Grant 1965
327       bee-fly          <NA>         <NA>              Grant,  Grant 1965
328       bee-fly          <NA>         <NA>              Grant,  Grant 1965
329       bee-fly          <NA>         <NA>              Grant,  Grant 1965
330       bee-fly          <NA>         <NA>              Grant,  Grant 1965
331       bee-fly          <NA>         <NA>              Grant,  Grant 1965
332       bee-fly          <NA>         <NA>              Grant,  Grant 1965
333       bee-fly          <NA>         <NA>              Grant,  Grant 1965
334       bee-fly          <NA>         <NA>              Grant,  Grant 1965
335           bee       bee-fly         <NA>              Grant,  Grant 1965
336           bee       bee-fly         <NA>              Grant,  Grant 1965
337           bee       bee-fly         <NA>              Grant,  Grant 1965
338           bee       bee-fly         <NA>              Grant,  Grant 1965
339           bee       bee-fly         <NA>              Grant,  Grant 1965
340           bee       bee-fly         <NA>              Grant,  Grant 1965
341           bee       bee-fly         <NA>              Grant,  Grant 1965
342           bee          <NA>         <NA>              Grant,  Grant 1965
343           bee          <NA>         <NA>              Grant,  Grant 1965
344           bee          <NA>         <NA>              Grant,  Grant 1965
345           bee          <NA>         <NA>              Grant,  Grant 1965
346           bee          <NA>         <NA>              Grant,  Grant 1965
347           bee          <NA>         <NA>              Grant,  Grant 1965
348          <NA>          <NA>         <NA>              Grant,  Grant 1965
349          <NA>          <NA>         <NA>              Grant,  Grant 1965
350          <NA>          <NA>         <NA>              Grant,  Grant 1965
351          <NA>          <NA>         <NA>              Grant,  Grant 1965
352          <NA>          <NA>         <NA>              Grant,  Grant 1965
353          <NA>          <NA>         <NA>              Grant,  Grant 1965
354          <NA>          <NA>         <NA>              Grant,  Grant 1965
355   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
356   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
357   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
358   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
359   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
360   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
361   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
362   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
363   hummingbird          <NA>         <NA>    Arizona-Sonora Desert Museum
364          <NA>          <NA>         <NA>              Grant,  Grant 1965
365          <NA>          <NA>         <NA>              Grant,  Grant 1965
366          <NA>          <NA>         <NA>              Grant,  Grant 1965
367          <NA>          <NA>         <NA>              Grant,  Grant 1965
368          <NA>          <NA>         <NA>              Grant,  Grant 1965
369          <NA>          <NA>         <NA>              Grant,  Grant 1965
370          <NA>          <NA>         <NA>              Grant,  Grant 1965
371          <NA>          <NA>         <NA>              Grant,  Grant 1965
372          <NA>          <NA>         <NA>              Grant,  Grant 1965
373           bee          <NA>         <NA>              Grant,  Grant 1965
374           bee          <NA>         <NA>              Grant,  Grant 1965
375           bee          <NA>         <NA>              Grant,  Grant 1965
376           bee          <NA>         <NA>              Grant,  Grant 1965
377           bee          <NA>         <NA>              Grant,  Grant 1965
378           bee          <NA>         <NA>              Grant,  Grant 1965
379           bee          <NA>         <NA>              Grant,  Grant 1965
380           bee          <NA>         <NA>              Grant,  Grant 1965
381           bee          <NA>         <NA>              Grant,  Grant 1965
382           bee          <NA>         <NA>              Grant,  Grant 1965
383           bee          <NA>         <NA>              Grant,  Grant 1965
384           bee          <NA>         <NA>              Grant,  Grant 1965
385          <NA>          <NA>         <NA>              Grant,  Grant 1965
386          <NA>          <NA>         <NA>              Grant,  Grant 1965
387          <NA>          <NA>         <NA>              Grant,  Grant 1965
388          <NA>          <NA>         <NA>              Grant,  Grant 1965
389          <NA>          <NA>         <NA>              Grant,  Grant 1965
390          <NA>          <NA>         <NA>              Grant,  Grant 1965
391       bee-fly          <NA>         <NA>              Grant,  Grant 1965
392       bee-fly          <NA>         <NA>              Grant,  Grant 1965
393       bee-fly          <NA>         <NA>              Grant,  Grant 1965
394       bee-fly          <NA>         <NA>              Grant,  Grant 1965
395       bee-fly          <NA>         <NA>              Grant,  Grant 1965
396       bee-fly          <NA>         <NA>              Grant,  Grant 1965
397           bee       bee-fly         <NA>              Grant,  Grant 1965
398           bee       bee-fly         <NA>              Grant,  Grant 1965
399           bee       bee-fly         <NA>              Grant,  Grant 1965
400           bee       bee-fly         <NA>              Grant,  Grant 1965
401           bee       bee-fly         <NA>              Grant,  Grant 1965
402           bee       bee-fly         <NA>              Grant,  Grant 1965
403           bee       bee-fly         <NA>              Grant,  Grant 1965
404           bee       bee-fly         <NA>              Grant,  Grant 1965
405           bee       bee-fly         <NA>              Grant,  Grant 1965
406           bee       bee-fly         <NA>              Grant,  Grant 1965
407           bee       bee-fly         <NA>              Grant,  Grant 1965
408           bee       bee-fly         <NA>              Grant,  Grant 1965
409           bee       bee-fly         <NA>              Grant,  Grant 1965
410           bee       bee-fly         <NA>              Grant,  Grant 1965
411           bee       bee-fly         <NA>              Grant,  Grant 1965
412           bee       bee-fly         <NA>              Grant,  Grant 1965
413           bee       bee-fly         <NA>              Grant,  Grant 1965
414           bee       bee-fly         <NA>              Grant,  Grant 1965
415           bee       bee-fly         <NA>              Grant,  Grant 1965
416           bee       bee-fly         <NA>              Grant,  Grant 1965
417           bee       bee-fly         <NA>              Grant,  Grant 1965
418           bee       bee-fly         <NA>              Grant,  Grant 1965
419           bee       bee-fly         <NA>              Grant,  Grant 1965
420           bee       bee-fly         <NA>              Grant,  Grant 1965
421          <NA>          <NA>         <NA>              Grant,  Grant 1965
422          <NA>          <NA>         <NA>              Grant,  Grant 1965
423          <NA>          <NA>         <NA>              Grant,  Grant 1965
424          <NA>          <NA>         <NA>              Grant,  Grant 1965
425          <NA>          <NA>         <NA>              Grant,  Grant 1965
426          <NA>          <NA>         <NA>              Grant,  Grant 1965
427          <NA>          <NA>         <NA>              Grant,  Grant 1965
428          <NA>          <NA>         <NA>              Grant,  Grant 1965
429          <NA>          <NA>         <NA>              Grant,  Grant 1965
430          <NA>          <NA>         <NA>              Grant,  Grant 1965
431          <NA>          <NA>         <NA>              Grant,  Grant 1965
432          <NA>          <NA>         <NA>              Grant,  Grant 1965
433          <NA>          <NA>         <NA>              Grant,  Grant 1965
434          <NA>          <NA>         <NA>              Grant,  Grant 1965
435          <NA>          <NA>         <NA>              Grant,  Grant 1965
436          <NA>          <NA>         <NA>              Grant,  Grant 1965
437          <NA>          <NA>         <NA>              Grant,  Grant 1965
438          <NA>          <NA>         <NA>              Grant,  Grant 1965
439          <NA>          <NA>         <NA>              Grant,  Grant 1965
440          <NA>          <NA>         <NA>              Grant,  Grant 1965
441          <NA>          <NA>         <NA>              Grant,  Grant 1965
442           bee       bee-fly         <NA>              Grant,  Grant 1965
443           bee       bee-fly         <NA>              Grant,  Grant 1965
444           bee       bee-fly         <NA>              Grant,  Grant 1965
445           bee       bee-fly         <NA>              Grant,  Grant 1965
446           bee       bee-fly         <NA>              Grant,  Grant 1965
447           bee       bee-fly         <NA>              Grant,  Grant 1965
448          <NA>          <NA>         <NA>              Grant,  Grant 1965
449          <NA>          <NA>         <NA>              Grant,  Grant 1965
450          <NA>          <NA>         <NA>              Grant,  Grant 1965
451          <NA>          <NA>         <NA>              Grant,  Grant 1965
452          <NA>          <NA>         <NA>              Grant,  Grant 1965
453          <NA>          <NA>         <NA>              Grant,  Grant 1965
454          <NA>          <NA>         <NA>              Grant,  Grant 1965
455          <NA>          <NA>         <NA>              Grant,  Grant 1965
456          <NA>          <NA>         <NA>              Grant,  Grant 1965
457          <NA>          <NA>         <NA>              Grant,  Grant 1965
458          <NA>          <NA>         <NA>              Grant,  Grant 1965
459          <NA>          <NA>         <NA>              Grant,  Grant 1965
460          <NA>          <NA>         <NA>              Grant,  Grant 1965
461          <NA>          <NA>         <NA>              Grant,  Grant 1965
462          <NA>          <NA>         <NA>              Grant,  Grant 1965
463          <NA>          <NA>         <NA>              Grant,  Grant 1965
464          <NA>          <NA>         <NA>              Grant,  Grant 1965
465          <NA>          <NA>         <NA>              Grant,  Grant 1965
466          <NA>          <NA>         <NA>              Grant,  Grant 1965
467          <NA>          <NA>         <NA>              Grant,  Grant 1965
468          <NA>          <NA>         <NA>              Grant,  Grant 1965
469          <NA>          <NA>         <NA>              Grant,  Grant 1965
470          <NA>          <NA>         <NA>              Grant,  Grant 1965
471          <NA>          <NA>         <NA>              Grant,  Grant 1965
472          <NA>          <NA>         <NA>              Grant,  Grant 1965
473          <NA>          <NA>         <NA>              Grant,  Grant 1965
474          <NA>          <NA>         <NA>              Grant,  Grant 1965
475          <NA>          <NA>         <NA>              Grant,  Grant 1965
476          <NA>          <NA>         <NA>              Grant,  Grant 1965
477          <NA>          <NA>         <NA>              Grant,  Grant 1965
478          <NA>          <NA>         <NA>              Grant,  Grant 1965
479          <NA>          <NA>         <NA>              Grant,  Grant 1965
480          <NA>          <NA>         <NA>              Grant,  Grant 1965
481           bee       bee-fly         <NA>              Grant,  Grant 1965
482           bee       bee-fly         <NA>              Grant,  Grant 1965
483           bee       bee-fly         <NA>              Grant,  Grant 1965
484           bee       bee-fly         <NA>              Grant,  Grant 1965
485           bee       bee-fly         <NA>              Grant,  Grant 1965
486           bee       bee-fly         <NA>              Grant,  Grant 1965
487           bee       bee-fly         <NA>              Grant,  Grant 1965
488           bee       bee-fly         <NA>              Grant,  Grant 1965
489           bee       bee-fly         <NA>              Grant,  Grant 1965
490           bee          <NA>         <NA>              Grant,  Grant 1965
491           bee          <NA>         <NA>              Grant,  Grant 1965
492           bee          <NA>         <NA>              Grant,  Grant 1965
493          <NA>          <NA>         <NA>              Grant,  Grant 1965
494          <NA>          <NA>         <NA>              Grant,  Grant 1965
495          <NA>          <NA>         <NA>              Grant,  Grant 1965
496          <NA>          <NA>         <NA>              Grant,  Grant 1965
497          <NA>          <NA>         <NA>              Grant,  Grant 1965
498          <NA>          <NA>         <NA>              Grant,  Grant 1965
499          <NA>          <NA>         <NA>              Grant,  Grant 1965
500          <NA>          <NA>         <NA>              Grant,  Grant 1965
501          <NA>          <NA>         <NA>              Grant,  Grant 1965
502          <NA>          <NA>         <NA>              Grant,  Grant 1965
503          <NA>          <NA>         <NA>              Grant,  Grant 1965
504          <NA>          <NA>         <NA>              Grant,  Grant 1965
505          <NA>          <NA>         <NA>              Grant,  Grant 1965
506          <NA>          <NA>         <NA>              Grant,  Grant 1965
507          <NA>          <NA>         <NA>              Grant,  Grant 1965
508          <NA>          <NA>         <NA>              Grant,  Grant 1965
509          <NA>          <NA>         <NA>              Grant,  Grant 1965
510          <NA>          <NA>         <NA>              Grant,  Grant 1965
511          <NA>          <NA>         <NA>              Grant,  Grant 1965
512          <NA>          <NA>         <NA>              Grant,  Grant 1965
513          <NA>          <NA>         <NA>              Grant,  Grant 1965
514          <NA>          <NA>         <NA>              Grant,  Grant 1965
515          <NA>          <NA>         <NA>              Grant,  Grant 1965
516          <NA>          <NA>         <NA>              Grant,  Grant 1965
517          <NA>          <NA>         <NA>              Grant,  Grant 1965
518          <NA>          <NA>         <NA>              Grant,  Grant 1965
519          <NA>          <NA>         <NA>              Grant,  Grant 1965
520          <NA>          <NA>         <NA>              Grant,  Grant 1965
521          <NA>          <NA>         <NA>              Grant,  Grant 1965
522          <NA>          <NA>         <NA>              Grant,  Grant 1965
523          <NA>          <NA>         <NA>              Grant,  Grant 1965
524          <NA>          <NA>         <NA>              Grant,  Grant 1965
525          <NA>          <NA>         <NA>              Grant,  Grant 1965
526          <NA>          <NA>         <NA>              Grant,  Grant 1965
527          <NA>          <NA>         <NA>              Grant,  Grant 1965
528          <NA>          <NA>         <NA>              Grant,  Grant 1965
529          <NA>          <NA>         <NA>              Grant,  Grant 1965
530          <NA>          <NA>         <NA>              Grant,  Grant 1965
531          <NA>          <NA>         <NA>              Grant,  Grant 1965
532          <NA>          <NA>         <NA>              Grant,  Grant 1965
533          <NA>          <NA>         <NA>              Grant,  Grant 1965
534          <NA>          <NA>         <NA>              Grant,  Grant 1965
535          <NA>          <NA>         <NA>              Grant,  Grant 1965
536          <NA>          <NA>         <NA>              Grant,  Grant 1965
537          <NA>          <NA>         <NA>              Grant,  Grant 1965
538       bee-fly          <NA>         <NA>              Grant,  Grant 1965
539       bee-fly          <NA>         <NA>              Grant,  Grant 1965
540       bee-fly          <NA>         <NA>              Grant,  Grant 1965
541       bee-fly          <NA>         <NA>              Grant,  Grant 1965
542       bee-fly          <NA>         <NA>              Grant,  Grant 1965
543       bee-fly          <NA>         <NA>              Grant,  Grant 1965
544       bee-fly          <NA>         <NA>              Grant,  Grant 1965
545       bee-fly          <NA>         <NA>              Grant,  Grant 1965
546       bee-fly          <NA>         <NA>              Grant,  Grant 1965
547       bee-fly          <NA>         <NA>              Grant,  Grant 1965
548       bee-fly          <NA>         <NA>              Grant,  Grant 1965
549       bee-fly          <NA>         <NA>              Grant,  Grant 1965
550       bee-fly          <NA>         <NA>              Grant,  Grant 1965
551       bee-fly          <NA>         <NA>              Grant,  Grant 1965
552       bee-fly          <NA>         <NA>              Grant,  Grant 1965
553       bee-fly          <NA>         <NA>              Grant,  Grant 1965
554       bee-fly          <NA>         <NA>              Grant,  Grant 1965
555       bee-fly          <NA>         <NA>              Grant,  Grant 1965
556          <NA>          <NA>         <NA>              Grant,  Grant 1965
557          <NA>          <NA>         <NA>              Grant,  Grant 1965
558          <NA>          <NA>         <NA>              Grant,  Grant 1965
559           bee       bee-fly         <NA>              Grant,  Grant 1965
560           bee       bee-fly         <NA>              Grant,  Grant 1965
561           bee       bee-fly         <NA>              Grant,  Grant 1965
562           bee       bee-fly         <NA>              Grant,  Grant 1965
563           bee       bee-fly         <NA>              Grant,  Grant 1965
564           bee       bee-fly         <NA>              Grant,  Grant 1965
565           bee       bee-fly         <NA>              Grant,  Grant 1965
566           bee       bee-fly         <NA>              Grant,  Grant 1965
567           bee       bee-fly         <NA>              Grant,  Grant 1965
568           bee       bee-fly         <NA>              Grant,  Grant 1965
569           bee       bee-fly         <NA>              Grant,  Grant 1965
570           bee       bee-fly         <NA>              Grant,  Grant 1965
571           bee       bee-fly         <NA>              Grant,  Grant 1965
572           bee       bee-fly         <NA>              Grant,  Grant 1965
573           bee       bee-fly         <NA>              Grant,  Grant 1965
574          <NA>          <NA>         <NA>              Grant,  Grant 1965
575          <NA>          <NA>         <NA>              Grant,  Grant 1965
576          <NA>          <NA>         <NA>              Grant,  Grant 1965
577          <NA>          <NA>         <NA>              Grant,  Grant 1965
578          <NA>          <NA>         <NA>              Grant,  Grant 1965
579          <NA>          <NA>         <NA>              Grant,  Grant 1965
580          <NA>          <NA>         <NA>              Grant,  Grant 1965
581          <NA>          <NA>         <NA>              Grant,  Grant 1965
582          <NA>          <NA>         <NA>              Grant,  Grant 1965
583          <NA>          <NA>         <NA>              Grant,  Grant 1965
584          <NA>          <NA>         <NA>              Grant,  Grant 1965
585          <NA>          <NA>         <NA>              Grant,  Grant 1965
586          <NA>          <NA>         <NA>              Grant,  Grant 1965
587          <NA>          <NA>         <NA>              Grant,  Grant 1965
588           bee          <NA>         <NA>              Grant,  Grant 1965
589           bee          <NA>         <NA>              Grant,  Grant 1965
590           bee          <NA>         <NA>              Grant,  Grant 1965
591           bee          <NA>         <NA>              Grant,  Grant 1965
592           bee          <NA>         <NA>              Grant,  Grant 1965
593           bee          <NA>         <NA>              Grant,  Grant 1965
594           bee          <NA>         <NA>              Grant,  Grant 1965
595           bee          <NA>         <NA>              Grant,  Grant 1965
596          <NA>          <NA>         <NA>              Grant,  Grant 1965
597          <NA>          <NA>         <NA>              Grant,  Grant 1965
598          <NA>          <NA>         <NA>              Grant,  Grant 1965
599          <NA>          <NA>         <NA>              Grant,  Grant 1965
600          <NA>          <NA>         <NA>              Grant,  Grant 1965
601          <NA>          <NA>         <NA>              Grant,  Grant 1965
602          <NA>          <NA>         <NA>              Grant,  Grant 1965
603          <NA>          <NA>         <NA>              Grant,  Grant 1965
604          <NA>          <NA>         <NA>              Grant,  Grant 1965
605          <NA>          <NA>         <NA>              Grant,  Grant 1965
606          <NA>          <NA>         <NA>              Grant,  Grant 1965
607          <NA>          <NA>         <NA>              Grant,  Grant 1965
608          <NA>          <NA>         <NA>              Grant,  Grant 1965
609          <NA>          <NA>         <NA>              Grant,  Grant 1965
610          <NA>          <NA>         <NA>              Grant,  Grant 1965
611          <NA>          <NA>         <NA>              Grant,  Grant 1965
612          <NA>          <NA>         <NA>              Grant,  Grant 1965
613          <NA>          <NA>         <NA>              Grant,  Grant 1965
614          <NA>          <NA>         <NA>              Grant,  Grant 1965
615          <NA>          <NA>         <NA>              Grant,  Grant 1965
616          <NA>          <NA>         <NA>              Grant,  Grant 1965
617          <NA>          <NA>         <NA>              Grant,  Grant 1965
618          <NA>          <NA>         <NA>              Grant,  Grant 1965
619          <NA>          <NA>         <NA>              Grant,  Grant 1965
620   hummingbird          <NA>         <NA>              Grant,  Grant 1965
621   hummingbird          <NA>         <NA>              Grant,  Grant 1965
622   hummingbird          <NA>         <NA>              Grant,  Grant 1965
623   hummingbird          <NA>         <NA>              Grant,  Grant 1965
624   hummingbird          <NA>         <NA>              Grant,  Grant 1965
625   hummingbird          <NA>         <NA>              Grant,  Grant 1965
626   hummingbird          <NA>         <NA>              Grant,  Grant 1965
627   hummingbird          <NA>         <NA>              Grant,  Grant 1965
628   hummingbird          <NA>         <NA>              Grant,  Grant 1965
629   hummingbird          <NA>         <NA>              Grant,  Grant 1965
630   hummingbird          <NA>         <NA>              Grant,  Grant 1965
631   hummingbird          <NA>         <NA>              Grant,  Grant 1965
632   hummingbird          <NA>         <NA>              Grant,  Grant 1965
633   hummingbird          <NA>         <NA>              Grant,  Grant 1965
634   hummingbird          <NA>         <NA>              Grant,  Grant 1965
635   hummingbird          <NA>         <NA>              Grant,  Grant 1965
636   hummingbird          <NA>         <NA>              Grant,  Grant 1965
637   hummingbird          <NA>         <NA>              Grant,  Grant 1965
638   hummingbird          <NA>         <NA>              Grant,  Grant 1965
639   hummingbird          <NA>         <NA>              Grant,  Grant 1965
640   hummingbird          <NA>         <NA>              Grant,  Grant 1965
641       beetles           bee         <NA>              Grant,  Grant 1965
642       beetles           bee         <NA>              Grant,  Grant 1965
643       beetles           bee         <NA>              Grant,  Grant 1965
644       beetles           bee         <NA>              Grant,  Grant 1965
645       beetles           bee         <NA>              Grant,  Grant 1965
646       beetles           bee         <NA>              Grant,  Grant 1965
647       beetles           bee         <NA>              Grant,  Grant 1965
648       beetles           bee         <NA>              Grant,  Grant 1965
649       beetles           bee         <NA>              Grant,  Grant 1965
650          <NA>          <NA>         <NA>              Grant,  Grant 1965
651          <NA>          <NA>         <NA>              Grant,  Grant 1965
652          <NA>          <NA>         <NA>              Grant,  Grant 1965
653          <NA>          <NA>         <NA>              Grant,  Grant 1965
654          <NA>          <NA>         <NA>              Grant,  Grant 1965
655          <NA>          <NA>         <NA>              Grant,  Grant 1965
656          <NA>          <NA>         <NA>              Grant,  Grant 1965
657          <NA>          <NA>         <NA>              Grant,  Grant 1965
658          <NA>          <NA>         <NA>              Grant,  Grant 1965
659       bee-fly          <NA>         <NA>                       Wood 2009
660       bee-fly          <NA>         <NA>                       Wood 2009
661       bee-fly          <NA>         <NA>                       Wood 2009
662       bee-fly          <NA>         <NA>                       Wood 2009
663       bee-fly          <NA>         <NA>                       Wood 2009
664       bee-fly          <NA>         <NA>                       Wood 2009
665       bee-fly          <NA>         <NA>                       Wood 2009
666       bee-fly          <NA>         <NA>                       Wood 2009
667       bee-fly          <NA>         <NA>                       Wood 2009
668      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
669      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
670      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
671      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
672      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
673      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
674      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
675      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
676      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
677      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
678      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
679      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
680      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
681      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
682      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
683      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
684      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
685      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
686      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
687      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
688      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
689     butterfly           bee         <NA>              Grant,  Grant 1965
690     butterfly           bee         <NA>              Grant,  Grant 1965
691     butterfly           bee         <NA>              Grant,  Grant 1965
692     butterfly           bee         <NA>              Grant,  Grant 1965
693     butterfly           bee         <NA>              Grant,  Grant 1965
694     butterfly           bee         <NA>              Grant,  Grant 1965
695     butterfly           bee         <NA>              Grant,  Grant 1965
696     butterfly           bee         <NA>              Grant,  Grant 1965
697     butterfly           bee         <NA>              Grant,  Grant 1965
698     butterfly           bee         <NA>              Grant,  Grant 1965
699     butterfly           bee         <NA>              Grant,  Grant 1965
700     butterfly           bee         <NA>              Grant,  Grant 1965
701     butterfly           bee         <NA>              Grant,  Grant 1965
702     butterfly           bee         <NA>              Grant,  Grant 1965
703     butterfly           bee         <NA>              Grant,  Grant 1965
704     butterfly           bee         <NA>              Grant,  Grant 1965
705     butterfly           bee         <NA>              Grant,  Grant 1965
706     butterfly           bee         <NA>              Grant,  Grant 1965
707           bee          <NA>         <NA>              Grant,  Grant 1965
708           bee          <NA>         <NA>              Grant,  Grant 1965
709           bee          <NA>         <NA>              Grant,  Grant 1965
710           bee          <NA>         <NA>              Grant,  Grant 1965
711           bee          <NA>         <NA>              Grant,  Grant 1965
712           bee          <NA>         <NA>              Grant,  Grant 1965
713           bee          <NA>         <NA>              Grant,  Grant 1965
714           bee          <NA>         <NA>              Grant,  Grant 1965
715           bee          <NA>         <NA>              Grant,  Grant 1965
716           bee          <NA>         <NA>              Grant,  Grant 1965
717           bee          <NA>         <NA>              Grant,  Grant 1965
718           bee          <NA>         <NA>              Grant,  Grant 1965
719           bee          <NA>         <NA>              Grant,  Grant 1965
720           bee          <NA>         <NA>              Grant,  Grant 1965
721           bee          <NA>         <NA>              Grant,  Grant 1965
722           bee          <NA>         <NA>              Grant,  Grant 1965
723           bee          <NA>         <NA>              Grant,  Grant 1965
724           bee          <NA>         <NA>              Grant,  Grant 1965
725           bee          <NA>         <NA>              Grant,  Grant 1965
726           bee          <NA>         <NA>              Grant,  Grant 1965
727           bee          <NA>         <NA>              Grant,  Grant 1965
728           bee          <NA>         <NA>              Grant,  Grant 1965
729           bee          <NA>         <NA>              Grant,  Grant 1965
730           bee          <NA>         <NA>              Grant,  Grant 1965
731           bee          <NA>         <NA>              Grant,  Grant 1965
732           bee          <NA>         <NA>              Grant,  Grant 1965
733           bee          <NA>         <NA>              Grant,  Grant 1965
734           bee          <NA>         <NA>              Grant,  Grant 1965
735           bee          <NA>         <NA>              Grant,  Grant 1965
736           bee          <NA>         <NA>              Grant,  Grant 1965
737           bee          <NA>         <NA>              Grant,  Grant 1965
738           bee          <NA>         <NA>              Grant,  Grant 1965
739           bee          <NA>         <NA>              Grant,  Grant 1965
740           bee          <NA>         <NA>              Grant,  Grant 1965
741           bee          <NA>         <NA>              Grant,  Grant 1965
742           bee          <NA>         <NA>              Grant,  Grant 1965
743           bee          <NA>         <NA>              Grant,  Grant 1965
744           bee          <NA>         <NA>              Grant,  Grant 1965
745           bee          <NA>         <NA>              Grant,  Grant 1965
746          <NA>          <NA>         <NA>              Grant,  Grant 1965
747          <NA>          <NA>         <NA>              Grant,  Grant 1965
748          <NA>          <NA>         <NA>              Grant,  Grant 1965
749          <NA>          <NA>         <NA>              Grant,  Grant 1965
750          <NA>          <NA>         <NA>              Grant,  Grant 1965
751          <NA>          <NA>         <NA>              Grant,  Grant 1965
752          <NA>          <NA>         <NA>              Grant,  Grant 1965
753          <NA>          <NA>         <NA>              Grant,  Grant 1965
754          <NA>          <NA>         <NA>              Grant,  Grant 1965
755          <NA>          <NA>         <NA>              Grant,  Grant 1965
756          <NA>          <NA>         <NA>              Grant,  Grant 1965
757          <NA>          <NA>         <NA>              Grant,  Grant 1965
758          <NA>          <NA>         <NA>              Grant,  Grant 1965
759          <NA>          <NA>         <NA>              Grant,  Grant 1965
760          <NA>          <NA>         <NA>              Grant,  Grant 1965
761          <NA>          <NA>         <NA>              Grant,  Grant 1965
762          <NA>          <NA>         <NA>              Grant,  Grant 1965
763          <NA>          <NA>         <NA>              Grant,  Grant 1965
764          <NA>          <NA>         <NA>              Grant,  Grant 1965
765          <NA>          <NA>         <NA>              Grant,  Grant 1965
766          <NA>          <NA>         <NA>              Grant,  Grant 1965
767          <NA>          <NA>         <NA>              Grant,  Grant 1965
768          <NA>          <NA>         <NA>              Grant,  Grant 1965
769          <NA>          <NA>         <NA>              Grant,  Grant 1965
770          <NA>          <NA>         <NA>              Grant,  Grant 1965
771          <NA>          <NA>         <NA>              Grant,  Grant 1965
772          <NA>          <NA>         <NA>              Grant,  Grant 1965
773          <NA>          <NA>         <NA>              Grant,  Grant 1965
774          <NA>          <NA>         <NA>              Grant,  Grant 1965
775          <NA>          <NA>         <NA>              Grant,  Grant 1965
776          <NA>          <NA>         <NA>              Grant,  Grant 1965
777          <NA>          <NA>         <NA>              Grant,  Grant 1965
778          <NA>          <NA>         <NA>              Grant,  Grant 1965
779          <NA>          <NA>         <NA>              Grant,  Grant 1965
780          <NA>          <NA>         <NA>              Grant,  Grant 1965
781          <NA>          <NA>         <NA>              Grant,  Grant 1965
782          <NA>          <NA>         <NA>              Grant,  Grant 1965
783          <NA>          <NA>         <NA>              Grant,  Grant 1965
784          <NA>          <NA>         <NA>              Grant,  Grant 1965
785          <NA>          <NA>         <NA>              Grant,  Grant 1965
786          <NA>          <NA>         <NA>              Grant,  Grant 1965
787          <NA>          <NA>         <NA>              Grant,  Grant 1965
788          <NA>          <NA>         <NA>              Grant,  Grant 1965
789          <NA>          <NA>         <NA>              Grant,  Grant 1965
790          <NA>          <NA>         <NA>              Grant,  Grant 1965
791          <NA>          <NA>         <NA>              Grant,  Grant 1965
792          <NA>          <NA>         <NA>              Grant,  Grant 1965
793   hummingbird          <NA>         <NA>              Grant,  Grant 1965
794   hummingbird          <NA>         <NA>              Grant,  Grant 1965
795   hummingbird          <NA>         <NA>              Grant,  Grant 1965
796   hummingbird          <NA>         <NA>              Grant,  Grant 1965
797   hummingbird          <NA>         <NA>              Grant,  Grant 1965
798   hummingbird          <NA>         <NA>              Grant,  Grant 1965
799   hummingbird          <NA>         <NA>              Grant,  Grant 1965
800   hummingbird          <NA>         <NA>              Grant,  Grant 1965
801      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
802      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
803      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
804      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
805      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
806      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
807      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
808      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
809      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
810      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
811      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
812      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
813      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
814      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
815      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
816      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
817      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
818      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
819      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
820      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
821      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
822       bee-fly          <NA>         <NA>              Grant,  Grant 1965
823       bee-fly          <NA>         <NA>              Grant,  Grant 1965
824       bee-fly          <NA>         <NA>              Grant,  Grant 1965
825       bee-fly          <NA>         <NA>              Grant,  Grant 1965
826       bee-fly          <NA>         <NA>              Grant,  Grant 1965
827       bee-fly          <NA>         <NA>              Grant,  Grant 1965
828       bee-fly          <NA>         <NA>              Grant,  Grant 1965
829       bee-fly          <NA>         <NA>              Grant,  Grant 1965
830       bee-fly          <NA>         <NA>              Grant,  Grant 1965
831       bee-fly          <NA>         <NA>              Grant,  Grant 1965
832       bee-fly          <NA>         <NA>              Grant,  Grant 1965
833       bee-fly          <NA>         <NA>              Grant,  Grant 1965
834       bee-fly          <NA>         <NA>              Grant,  Grant 1965
835       bee-fly          <NA>         <NA>              Grant,  Grant 1965
836       bee-fly          <NA>         <NA>              Grant,  Grant 1965
837       bee-fly          <NA>         <NA>              Grant,  Grant 1965
838       bee-fly          <NA>         <NA>              Grant,  Grant 1965
839       bee-fly          <NA>         <NA>              Grant,  Grant 1965
840       bee-fly          <NA>         <NA>              Grant,  Grant 1965
841       bee-fly          <NA>         <NA>              Grant,  Grant 1965
842       bee-fly          <NA>         <NA>              Grant,  Grant 1965
843       bee-fly          <NA>         <NA>              Grant,  Grant 1965
844         flies          <NA>         <NA>              Grant,  Grant 1965
845         flies          <NA>         <NA>              Grant,  Grant 1965
846         flies          <NA>         <NA>              Grant,  Grant 1965
847         flies          <NA>         <NA>              Grant,  Grant 1965
848         flies          <NA>         <NA>              Grant,  Grant 1965
849         flies          <NA>         <NA>              Grant,  Grant 1965
850         flies          <NA>         <NA>              Grant,  Grant 1965
851         flies          <NA>         <NA>              Grant,  Grant 1965
852         flies          <NA>         <NA>              Grant,  Grant 1965
853       bee-fly          <NA>         <NA>              Grant,  Grant 1965
854       bee-fly          <NA>         <NA>              Grant,  Grant 1965
855       bee-fly          <NA>         <NA>              Grant,  Grant 1965
856       bee-fly          <NA>         <NA>              Grant,  Grant 1965
857       bee-fly          <NA>         <NA>              Grant,  Grant 1965
858       bee-fly          <NA>         <NA>              Grant,  Grant 1965
859       bee-fly          <NA>         <NA>              Grant,  Grant 1965
860       bee-fly          <NA>         <NA>              Grant,  Grant 1965
861       bee-fly          <NA>         <NA>              Grant,  Grant 1965
862       bee-fly          <NA>         <NA>              Grant,  Grant 1965
863       bee-fly          <NA>         <NA>              Grant,  Grant 1965
864       bee-fly          <NA>         <NA>              Grant,  Grant 1965
865       bee-fly          <NA>         <NA>              Grant,  Grant 1965
866       bee-fly          <NA>         <NA>              Grant,  Grant 1965
867       bee-fly          <NA>         <NA>              Grant,  Grant 1965
868          <NA>          <NA>         <NA>              Grant,  Grant 1965
869          <NA>          <NA>         <NA>              Grant,  Grant 1965
870          <NA>          <NA>         <NA>              Grant,  Grant 1965
871           bee          <NA>         <NA>              Grant,  Grant 1965
872           bee          <NA>         <NA>              Grant,  Grant 1965
873           bee          <NA>         <NA>              Grant,  Grant 1965
874           bee          <NA>         <NA>              Grant,  Grant 1965
875           bee          <NA>         <NA>              Grant,  Grant 1965
876           bee          <NA>         <NA>              Grant,  Grant 1965
877          <NA>          <NA>         <NA>              Grant,  Grant 1965
878          <NA>          <NA>         <NA>              Grant,  Grant 1965
879          <NA>          <NA>         <NA>              Grant,  Grant 1965
880          <NA>          <NA>         <NA>              Grant,  Grant 1965
881          <NA>          <NA>         <NA>              Grant,  Grant 1965
882          <NA>          <NA>         <NA>              Grant,  Grant 1965
883           bee       bee-fly         <NA>              Grant,  Grant 1965
884           bee       bee-fly         <NA>              Grant,  Grant 1965
885           bee       bee-fly         <NA>              Grant,  Grant 1965
886     butterfly       bee-fly         <NA>              Grant,  Grant 1965
887     butterfly       bee-fly         <NA>              Grant,  Grant 1965
888     butterfly       bee-fly         <NA>              Grant,  Grant 1965
889     butterfly       bee-fly         <NA>              Grant,  Grant 1965
890     butterfly       bee-fly         <NA>              Grant,  Grant 1965
891     butterfly       bee-fly         <NA>              Grant,  Grant 1965
892     butterfly       bee-fly         <NA>              Grant,  Grant 1965
893     butterfly       bee-fly         <NA>              Grant,  Grant 1965
894     butterfly       bee-fly         <NA>              Grant,  Grant 1965
895     butterfly       bee-fly         <NA>              Grant,  Grant 1965
896     butterfly       bee-fly         <NA>              Grant,  Grant 1965
897     butterfly       bee-fly         <NA>              Grant,  Grant 1965
898       bee-fly          <NA>         <NA>              Grant,  Grant 1965
899       bee-fly          <NA>         <NA>              Grant,  Grant 1965
900       bee-fly          <NA>         <NA>              Grant,  Grant 1965
901       bee-fly          <NA>         <NA>              Grant,  Grant 1965
902       bee-fly          <NA>         <NA>              Grant,  Grant 1965
903       bee-fly          <NA>         <NA>              Grant,  Grant 1965
904          <NA>          <NA>         <NA>              Grant,  Grant 1965
905          <NA>          <NA>         <NA>              Grant,  Grant 1965
906          <NA>          <NA>         <NA>              Grant,  Grant 1965
907          <NA>          <NA>         <NA>              Grant,  Grant 1965
908          <NA>          <NA>         <NA>              Grant,  Grant 1965
909          <NA>          <NA>         <NA>              Grant,  Grant 1965
910          <NA>          <NA>         <NA>              Grant,  Grant 1965
911          <NA>          <NA>         <NA>              Grant,  Grant 1965
912          <NA>          <NA>         <NA>              Grant,  Grant 1965
913          <NA>          <NA>         <NA>              Grant,  Grant 1965
914          <NA>          <NA>         <NA>              Grant,  Grant 1965
915          <NA>          <NA>         <NA>              Grant,  Grant 1965
916     butterfly    hawkmoths,        flies              Grant,  Grant 1965
917     butterfly    hawkmoths,        flies              Grant,  Grant 1965
918     butterfly    hawkmoths,        flies              Grant,  Grant 1965
919     butterfly    hawkmoths,        flies              Grant,  Grant 1965
920     butterfly    hawkmoths,        flies              Grant,  Grant 1965
921     butterfly    hawkmoths,        flies              Grant,  Grant 1965
922     butterfly    hawkmoths,        flies              Grant,  Grant 1965
923     butterfly    hawkmoths,        flies              Grant,  Grant 1965
924     butterfly    hawkmoths,        flies              Grant,  Grant 1965
925     butterfly    hawkmoths,        flies              Grant,  Grant 1965
926     butterfly    hawkmoths,        flies              Grant,  Grant 1965
927     butterfly    hawkmoths,        flies              Grant,  Grant 1965
928     butterfly    hawkmoths,        flies              Grant,  Grant 1965
929     butterfly    hawkmoths,        flies              Grant,  Grant 1965
930     butterfly    hawkmoths,        flies              Grant,  Grant 1965
931     butterfly    hawkmoths,        flies              Grant,  Grant 1965
932     butterfly    hawkmoths,        flies              Grant,  Grant 1965
933     butterfly    hawkmoths,        flies              Grant,  Grant 1965
934           bee       beetles         <NA>              Grant,  Grant 1965
935           bee       beetles         <NA>              Grant,  Grant 1965
936           bee       beetles         <NA>              Grant,  Grant 1965
937           bee       beetles         <NA>              Grant,  Grant 1965
938           bee       beetles         <NA>              Grant,  Grant 1965
939           bee       beetles         <NA>              Grant,  Grant 1965
940           bee       beetles         <NA>              Grant,  Grant 1965
941           bee       beetles         <NA>              Grant,  Grant 1965
942           bee       beetles         <NA>              Grant,  Grant 1965
943      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
944      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
945      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
946      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
947      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
948      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
949      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
950      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
951      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
952      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
953      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
954      hawkmoth          <NA>         <NA>              Grant,  Grant 1965
955        beetle          <NA>         <NA>              Grant,  Grant 1965
956        beetle          <NA>         <NA>              Grant,  Grant 1965
957        beetle          <NA>         <NA>              Grant,  Grant 1965
958        beetle          <NA>         <NA>              Grant,  Grant 1965
959        beetle          <NA>         <NA>              Grant,  Grant 1965
960        beetle          <NA>         <NA>              Grant,  Grant 1965
961   hummingbird          <NA>         <NA>              Grant,  Grant 1965
962   hummingbird          <NA>         <NA>              Grant,  Grant 1965
963   hummingbird          <NA>         <NA>              Grant,  Grant 1965
964          <NA>          <NA>         <NA>              Grant,  Grant 1965
965          <NA>          <NA>         <NA>              Grant,  Grant 1965
966          <NA>          <NA>         <NA>              Grant,  Grant 1965
967          <NA>          <NA>         <NA>              Grant,  Grant 1965
968          <NA>          <NA>         <NA>              Grant,  Grant 1965
969          <NA>          <NA>         <NA>              Grant,  Grant 1965
970       bee-fly           bee         <NA>              Grant,  Grant 1965
971       bee-fly           bee         <NA>              Grant,  Grant 1965
972       bee-fly           bee         <NA>              Grant,  Grant 1965
973       bee-fly           bee         <NA>              Grant,  Grant 1965
974       bee-fly           bee         <NA>              Grant,  Grant 1965
975       bee-fly           bee         <NA>              Grant,  Grant 1965
976       bee-fly           bee         <NA>              Grant,  Grant 1965
977       bee-fly           bee         <NA>              Grant,  Grant 1965
978       bee-fly           bee         <NA>              Grant,  Grant 1965
979       bee-fly           bee         <NA>              Grant,  Grant 1965
980       bee-fly           bee         <NA>              Grant,  Grant 1965
981       bee-fly           bee         <NA>              Grant,  Grant 1965
982       bee-fly           bee         <NA>              Grant,  Grant 1965
983       bee-fly           bee         <NA>              Grant,  Grant 1965
984       bee-fly           bee         <NA>              Grant,  Grant 1965
985          <NA>          <NA>         <NA>              Grant,  Grant 1965
986          <NA>          <NA>         <NA>              Grant,  Grant 1965
987          <NA>          <NA>         <NA>              Grant,  Grant 1965
988          <NA>          <NA>         <NA>              Grant,  Grant 1965
989          <NA>          <NA>         <NA>              Grant,  Grant 1965
990          <NA>          <NA>         <NA>              Grant,  Grant 1965
991          <NA>          <NA>         <NA>              Grant,  Grant 1965
992          <NA>          <NA>         <NA>              Grant,  Grant 1965
993          <NA>          <NA>         <NA>              Grant,  Grant 1965
994          <NA>          <NA>         <NA>              Grant,  Grant 1965
995          <NA>          <NA>         <NA>              Grant,  Grant 1965
996          <NA>          <NA>         <NA>              Grant,  Grant 1965
997          <NA>          <NA>         <NA>              Grant,  Grant 1965
998          <NA>          <NA>         <NA>              Grant,  Grant 1965
999          <NA>          <NA>         <NA>              Grant,  Grant 1965
1000         <NA>          <NA>         <NA>              Grant,  Grant 1965
1001         <NA>          <NA>         <NA>              Grant,  Grant 1965
1002          bee       bee-fly         <NA>              Grant,  Grant 1965
1003          bee       bee-fly         <NA>              Grant,  Grant 1965
1004          bee       bee-fly         <NA>              Grant,  Grant 1965
1005         <NA>          <NA>         <NA>              Grant,  Grant 1965
1006         <NA>          <NA>         <NA>              Grant,  Grant 1965
1007         <NA>          <NA>         <NA>              Grant,  Grant 1965
1008         <NA>          <NA>         <NA>              Grant,  Grant 1965
1009         <NA>          <NA>         <NA>              Grant,  Grant 1965
1010         <NA>          <NA>         <NA>              Grant,  Grant 1965
1011      bee-fly           bee         <NA>              Grant,  Grant 1965
1012      bee-fly           bee         <NA>              Grant,  Grant 1965
1013          bee       bee-fly         <NA>              Grant,  Grant 1965
1014          bee       bee-fly         <NA>              Grant,  Grant 1965
1015          bee       bee-fly         <NA>              Grant,  Grant 1965
1016          bee       bee-fly         <NA>              Grant,  Grant 1965
1017          bee       bee-fly         <NA>              Grant,  Grant 1965
1018          bee       bee-fly         <NA>              Grant,  Grant 1965
1019          bee       bee-fly         <NA>              Grant,  Grant 1965
1020          bee       bee-fly         <NA>              Grant,  Grant 1965
1021          bee       bee-fly         <NA>              Grant,  Grant 1965
1022          bee       bee-fly         <NA>              Grant,  Grant 1965
1023          bee       bee-fly         <NA>              Grant,  Grant 1965
1024          bee       bee-fly         <NA>              Grant,  Grant 1965
1025      bee-fly          <NA>         <NA>              Grant,  Grant 1965
1026      bee-fly          <NA>         <NA>              Grant,  Grant 1965
1027      bee-fly          <NA>         <NA>              Grant,  Grant 1965
1028          bee          <NA>         <NA>              Grant,  Grant 1965
1029          bee          <NA>         <NA>              Grant,  Grant 1965
1030          bee          <NA>         <NA>              Grant,  Grant 1965
1031          bee          <NA>         <NA>              Grant,  Grant 1965
1032          bee          <NA>         <NA>              Grant,  Grant 1965
1033          bee          <NA>         <NA>              Grant,  Grant 1965
1034         <NA>          <NA>         <NA>              Grant,  Grant 1965
1035         <NA>          <NA>         <NA>              Grant,  Grant 1965
1036         <NA>          <NA>         <NA>              Grant,  Grant 1965
1037         <NA>          <NA>         <NA>              Grant,  Grant 1965
1038         <NA>          <NA>         <NA>              Grant,  Grant 1965
1039         <NA>          <NA>         <NA>              Grant,  Grant 1965
1040         <NA>          <NA>         <NA>              Grant,  Grant 1965
1041         <NA>          <NA>         <NA>              Grant,  Grant 1965
1042         <NA>          <NA>         <NA>              Grant,  Grant 1965
1043         <NA>          <NA>         <NA>              Grant,  Grant 1965
1044         <NA>          <NA>         <NA>              Grant,  Grant 1965
1045         <NA>          <NA>         <NA>              Grant,  Grant 1965
1046         <NA>          <NA>         <NA>              Grant,  Grant 1965
1047         <NA>          <NA>         <NA>              Grant,  Grant 1965
1048         <NA>          <NA>         <NA>              Grant,  Grant 1965
1049          bee          <NA>         <NA>              Grant,  Grant 1965
1050          bee          <NA>         <NA>              Grant,  Grant 1965
1051          bee          <NA>         <NA>              Grant,  Grant 1965
1052          bee          <NA>         <NA>              Grant,  Grant 1965
1053          bee          <NA>         <NA>              Grant,  Grant 1965
1054          bee          <NA>         <NA>              Grant,  Grant 1965
1055    butterfly          <NA>         <NA>              Grant,  Grant 1965
1056    butterfly          <NA>         <NA>              Grant,  Grant 1965
1057    butterfly          <NA>         <NA>              Grant,  Grant 1965
1058    butterfly          <NA>         <NA>              Grant,  Grant 1965
1059    butterfly          <NA>         <NA>              Grant,  Grant 1965
1060    butterfly          <NA>         <NA>              Grant,  Grant 1965
1061    butterfly          <NA>         <NA>              Grant,  Grant 1965
1062    butterfly          <NA>         <NA>              Grant,  Grant 1965
1063    butterfly          <NA>         <NA>              Grant,  Grant 1965
1064     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1065     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1066     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1067     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1068     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1069     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1070         <NA>          <NA>         <NA>              Grant,  Grant 1965
1071         <NA>          <NA>         <NA>              Grant,  Grant 1965
1072         <NA>          <NA>         <NA>              Grant,  Grant 1965
1073         <NA>          <NA>         <NA>              Grant,  Grant 1965
1074         <NA>          <NA>         <NA>              Grant,  Grant 1965
1075         <NA>          <NA>         <NA>              Grant,  Grant 1965
1076         <NA>          <NA>         <NA>              Grant,  Grant 1965
1077         <NA>          <NA>         <NA>              Grant,  Grant 1965
1078         <NA>          <NA>         <NA>              Grant,  Grant 1965
1079         <NA>          <NA>         <NA>              Grant,  Grant 1965
1080         <NA>          <NA>         <NA>              Grant,  Grant 1965
1081         <NA>          <NA>         <NA>              Grant,  Grant 1965
1082         <NA>          <NA>         <NA>              Grant,  Grant 1965
1083         <NA>          <NA>         <NA>              Grant,  Grant 1965
1084         <NA>          <NA>         <NA>              Grant,  Grant 1965
1085     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1086     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1087     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1088     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1089     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1090     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1091     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1092     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1093     hawkmoth     butterfly         <NA>              Grant,  Grant 1965
1094    butterfly          <NA>         <NA>              Grant,  Grant 1965
1095    butterfly          <NA>         <NA>              Grant,  Grant 1965
1096    butterfly          <NA>         <NA>              Grant,  Grant 1965
1097    butterfly          <NA>         <NA>              Grant,  Grant 1965
1098    butterfly          <NA>         <NA>              Grant,  Grant 1965
1099    butterfly          <NA>         <NA>              Grant,  Grant 1965
1100    butterfly          <NA>         <NA>              Grant,  Grant 1965
1101    butterfly          <NA>         <NA>              Grant,  Grant 1965
1102    butterfly          <NA>         <NA>              Grant,  Grant 1965
1103    butterfly          <NA>         <NA>              Grant,  Grant 1965
1104    butterfly          <NA>         <NA>              Grant,  Grant 1965
1105    butterfly          <NA>         <NA>              Grant,  Grant 1965
1106    butterfly          <NA>         <NA>              Grant,  Grant 1965
1107    butterfly          <NA>         <NA>              Grant,  Grant 1965
1108    butterfly          <NA>         <NA>              Grant,  Grant 1965
1109    butterfly          <NA>         <NA>              Grant,  Grant 1965
1110    butterfly          <NA>         <NA>              Grant,  Grant 1965
1111    butterfly          <NA>         <NA>              Grant,  Grant 1965
1112    butterfly          <NA>         <NA>              Grant,  Grant 1965
1113    butterfly          <NA>         <NA>              Grant,  Grant 1965
1114    butterfly          <NA>         <NA>              Grant,  Grant 1965
1115    butterfly          <NA>         <NA>              Grant,  Grant 1965
1116    butterfly          <NA>         <NA>              Grant,  Grant 1965
1117     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1118     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1119     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1120     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1121     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1122     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1123    butterfly          <NA>         <NA>              Grant,  Grant 1965
1124    butterfly          <NA>         <NA>              Grant,  Grant 1965
1125    butterfly          <NA>         <NA>              Grant,  Grant 1965
1126    butterfly          <NA>         <NA>              Grant,  Grant 1965
1127    butterfly          <NA>         <NA>              Grant,  Grant 1965
1128    butterfly          <NA>         <NA>              Grant,  Grant 1965
1129    butterfly          <NA>         <NA>              Grant,  Grant 1965
1130    butterfly          <NA>         <NA>              Grant,  Grant 1965
1131    butterfly          <NA>         <NA>              Grant,  Grant 1965
1132    butterfly          <NA>         <NA>              Grant,  Grant 1965
1133    butterfly          <NA>         <NA>              Grant,  Grant 1965
1134    butterfly          <NA>         <NA>              Grant,  Grant 1965
1135    butterfly          <NA>         <NA>              Grant,  Grant 1965
1136    butterfly          <NA>         <NA>              Grant,  Grant 1965
1137    butterfly          <NA>         <NA>              Grant,  Grant 1965
1138    butterfly          <NA>         <NA>              Grant,  Grant 1965
1139    butterfly          <NA>         <NA>              Grant,  Grant 1965
1140    butterfly          <NA>         <NA>              Grant,  Grant 1965
1141    butterfly          <NA>         <NA>              Grant,  Grant 1965
1142    butterfly          <NA>         <NA>              Grant,  Grant 1965
1143    butterfly          <NA>         <NA>              Grant,  Grant 1965
1144    butterfly          <NA>         <NA>              Grant,  Grant 1965
1145    butterfly          <NA>         <NA>              Grant,  Grant 1965
1146    butterfly          <NA>         <NA>              Grant,  Grant 1965
1147    butterfly          <NA>         <NA>              Grant,  Grant 1965
1148    butterfly          <NA>         <NA>              Grant,  Grant 1965
1149    butterfly          <NA>         <NA>              Grant,  Grant 1965
1150    butterfly          <NA>         <NA>              Grant,  Grant 1965
1151    butterfly          <NA>         <NA>              Grant,  Grant 1965
1152    butterfly          <NA>         <NA>              Grant,  Grant 1965
1153    butterfly          <NA>         <NA>              Grant,  Grant 1965
1154    butterfly          <NA>         <NA>              Grant,  Grant 1965
1155    butterfly          <NA>         <NA>              Grant,  Grant 1965
1156    butterfly          <NA>         <NA>              Grant,  Grant 1965
1157    butterfly          <NA>         <NA>              Grant,  Grant 1965
1158    butterfly          <NA>         <NA>              Grant,  Grant 1965
1159    butterfly          <NA>         <NA>              Grant,  Grant 1965
1160    butterfly          <NA>         <NA>              Grant,  Grant 1965
1161    butterfly          <NA>         <NA>              Grant,  Grant 1965
1162    butterfly          <NA>         <NA>              Grant,  Grant 1965
1163    butterfly          <NA>         <NA>              Grant,  Grant 1965
1164    butterfly          <NA>         <NA>              Grant,  Grant 1965
1165    butterfly          <NA>         <NA>              Grant,  Grant 1965
1166    butterfly          <NA>         <NA>              Grant,  Grant 1965
1167    butterfly          <NA>         <NA>              Grant,  Grant 1965
1168    butterfly          <NA>         <NA>              Grant,  Grant 1965
1169    butterfly          <NA>         <NA>              Grant,  Grant 1965
1170    butterfly          <NA>         <NA>              Grant,  Grant 1965
1171    butterfly          <NA>         <NA>              Grant,  Grant 1965
1172    butterfly          <NA>         <NA>              Grant,  Grant 1965
1173    butterfly          <NA>         <NA>              Grant,  Grant 1965
1174    butterfly          <NA>         <NA>              Grant,  Grant 1965
1175    butterfly          <NA>         <NA>              Grant,  Grant 1965
1176    butterfly          <NA>         <NA>              Grant,  Grant 1965
1177    butterfly          <NA>         <NA>              Grant,  Grant 1965
1178    butterfly          <NA>         <NA>              Grant,  Grant 1965
1179    butterfly          <NA>         <NA>              Grant,  Grant 1965
1180    butterfly          <NA>         <NA>              Grant,  Grant 1965
1181    butterfly          <NA>         <NA>              Grant,  Grant 1965
1182    butterfly          <NA>         <NA>              Grant,  Grant 1965
1183    butterfly          <NA>         <NA>              Grant,  Grant 1965
1184    butterfly          <NA>         <NA>              Grant,  Grant 1965
1185    butterfly          <NA>         <NA>              Grant,  Grant 1965
1186    butterfly          <NA>         <NA>              Grant,  Grant 1965
1187    butterfly          <NA>         <NA>              Grant,  Grant 1965
1188    butterfly          <NA>         <NA>              Grant,  Grant 1965
1189    butterfly          <NA>         <NA>              Grant,  Grant 1965
1190    butterfly          <NA>         <NA>              Grant,  Grant 1965
1191    butterfly          <NA>         <NA>              Grant,  Grant 1965
1192    butterfly          <NA>         <NA>              Grant,  Grant 1965
1193    butterfly          <NA>         <NA>              Grant,  Grant 1965
1194    butterfly          <NA>         <NA>              Grant,  Grant 1965
1195    butterfly          <NA>         <NA>              Grant,  Grant 1965
1196    butterfly          <NA>         <NA>              Grant,  Grant 1965
1197    butterfly          <NA>         <NA>              Grant,  Grant 1965
1198    butterfly          <NA>         <NA>              Grant,  Grant 1965
1199    butterfly          <NA>         <NA>              Grant,  Grant 1965
1200    butterfly          <NA>         <NA>              Grant,  Grant 1965
1201    butterfly          <NA>         <NA>              Grant,  Grant 1965
1202    butterfly          <NA>         <NA>              Grant,  Grant 1965
1203    butterfly          <NA>         <NA>              Grant,  Grant 1965
1204    butterfly          <NA>         <NA>              Grant,  Grant 1965
1205     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1206     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1207     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1208     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1209     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1210     hawkmoth          <NA>         <NA>              Grant,  Grant 1965
1211    butterfly          <NA>         <NA>              Grant,  Grant 1965
1212    butterfly          <NA>         <NA>              Grant,  Grant 1965
1213    butterfly          <NA>         <NA>              Grant,  Grant 1965
1214    butterfly          <NA>         <NA>              Grant,  Grant 1965
1215    butterfly          <NA>         <NA>              Grant,  Grant 1965
1216    butterfly          <NA>         <NA>              Grant,  Grant 1965
1217    butterfly          <NA>         <NA>              Grant,  Grant 1965
1218    butterfly          <NA>         <NA>              Grant,  Grant 1965
1219    butterfly          <NA>         <NA>              Grant,  Grant 1965
1220    butterfly          <NA>         <NA>              Grant,  Grant 1965
1221    butterfly          <NA>         <NA>              Grant,  Grant 1965
1222    butterfly          <NA>         <NA>              Grant,  Grant 1965
1223    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1224    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1225    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1226    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1227    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1228    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1229    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1230    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1231    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1232    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1233    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1234    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1235    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1236    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1237    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1238    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1239    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1240    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1241    butterfly       bee-fly         <NA>              Grant,  Grant 1965
1242          bee         flies         <NA>              Grant,  Grant 1965
1243          bee         flies         <NA>              Grant,  Grant 1965
1244          bee         flies         <NA>              Grant,  Grant 1965
1245          bee          <NA>         <NA>              Grant,  Grant 1965
1246          bee          <NA>         <NA>              Grant,  Grant 1965
1247        flies           bee         <NA>              Grant,  Grant 1965
1248        flies           bee         <NA>              Grant,  Grant 1965
1249        flies           bee         <NA>              Grant,  Grant 1965
1250        flies           bee         <NA>              Grant,  Grant 1965
1251        flies           bee         <NA>              Grant,  Grant 1965
1252        flies           bee         <NA>              Grant,  Grant 1965
1253        flies           bee         <NA>              Grant,  Grant 1965
1254        flies           bee         <NA>              Grant,  Grant 1965
1255        flies           bee         <NA>              Grant,  Grant 1965
1256          bee  hummingbirds         <NA>              Grant,  Grant 1965
1257          bee  hummingbirds         <NA>              Grant,  Grant 1965
1258          bee  hummingbirds         <NA>              Grant,  Grant 1965
1259          bee  hummingbirds         <NA>              Grant,  Grant 1965
1260          bee  hummingbirds         <NA>              Grant,  Grant 1965
1261          bee  hummingbirds         <NA>              Grant,  Grant 1965
1262          bee  hummingbirds         <NA>              Grant,  Grant 1965
1263          bee  hummingbirds         <NA>              Grant,  Grant 1965
1264          bee  hummingbirds         <NA>              Grant,  Grant 1965
1265          bee  hummingbirds         <NA>              Grant,  Grant 1965
1266          bee  hummingbirds         <NA>              Grant,  Grant 1965
1267          bee  hummingbirds         <NA>              Grant,  Grant 1965
1268          bee  hummingbirds         <NA>              Grant,  Grant 1965
1269        flies           bee         <NA>              Grant,  Grant 1965
1270        flies           bee         <NA>              Grant,  Grant 1965
1271        flies           bee         <NA>              Grant,  Grant 1965
1272        flies           bee         <NA>              Grant,  Grant 1965
1273        flies           bee         <NA>              Grant,  Grant 1965
1274        flies           bee         <NA>              Grant,  Grant 1965
1275        flies           bee         <NA>              Grant,  Grant 1965
1276        flies           bee         <NA>              Grant,  Grant 1965
1277        flies          <NA>         <NA>              Grant,  Grant 1965
1278        flies          <NA>         <NA>              Grant,  Grant 1965
1279        flies          <NA>         <NA>              Grant,  Grant 1965
1280        flies          <NA>         <NA>              Grant,  Grant 1965
1281        flies          <NA>         <NA>              Grant,  Grant 1965
1282        flies          <NA>         <NA>              Grant,  Grant 1965
1283        flies          <NA>         <NA>              Grant,  Grant 1965
1284        flies          <NA>         <NA>              Grant,  Grant 1965
1285        flies          <NA>         <NA>              Grant,  Grant 1965
1286        flies          <NA>         <NA>              Grant,  Grant 1965
1287        flies          <NA>         <NA>              Grant,  Grant 1965
1288        flies          <NA>         <NA>              Grant,  Grant 1965
1289          bee          <NA>         <NA>              Grant,  Grant 1965
1290          bee          <NA>         <NA>              Grant,  Grant 1965
1291          bee          <NA>         <NA>              Grant,  Grant 1965
1292          bee          <NA>         <NA>              Grant,  Grant 1965
1293          bee          <NA>         <NA>              Grant,  Grant 1965
1294          bee          <NA>         <NA>              Grant,  Grant 1965
1295          bee          <NA>         <NA>              Grant,  Grant 1965
1296          bee          <NA>         <NA>              Grant,  Grant 1965
1297          bee          <NA>         <NA>              Grant,  Grant 1965
1298          bee          <NA>         <NA>              Grant,  Grant 1965
1299          bee          <NA>         <NA>              Grant,  Grant 1965
1300          bee          <NA>         <NA>              Grant,  Grant 1965
1301          bee          <NA>         <NA>              Grant,  Grant 1965
1302          bee          <NA>         <NA>              Grant,  Grant 1965
1303          bee          <NA>         <NA>              Grant,  Grant 1965
1304          bee          <NA>         <NA>              Grant,  Grant 1965
1305          bee          <NA>         <NA>              Grant,  Grant 1965
1306          bee          <NA>         <NA>              Grant,  Grant 1965
1307          bee          <NA>         <NA>              Grant,  Grant 1965
1308          bee          <NA>         <NA>              Grant,  Grant 1965
1309         <NA>          <NA>         <NA>              Grant,  Grant 1965
1310         <NA>          <NA>         <NA>              Grant,  Grant 1965
1311         <NA>          <NA>         <NA>              Grant,  Grant 1965
1312         <NA>          <NA>         <NA>              Grant,  Grant 1965
1313         <NA>          <NA>         <NA>              Grant,  Grant 1965
1314         <NA>          <NA>         <NA>              Grant,  Grant 1965
1315         <NA>          <NA>         <NA>              Grant,  Grant 1965
1316         <NA>          <NA>         <NA>              Grant,  Grant 1965
1317         <NA>          <NA>         <NA>              Grant,  Grant 1965
1318         <NA>          <NA>         <NA>              Grant,  Grant 1965
1319         <NA>          <NA>         <NA>              Grant,  Grant 1965
1320         <NA>          <NA>         <NA>              Grant,  Grant 1965
1321         <NA>          <NA>         <NA>              Grant,  Grant 1965
1322         <NA>          <NA>         <NA>              Grant,  Grant 1965
1323         <NA>          <NA>         <NA>              Grant,  Grant 1965
1324         <NA>          <NA>         <NA>              Grant,  Grant 1965
1325  hummingbird          <NA>         <NA>              Grant,  Grant 1965
1326  hummingbird          <NA>         <NA>              Grant,  Grant 1965
1327          bee         flies         <NA>              Grant,  Grant 1965
1328          bee         flies         <NA>              Grant,  Grant 1965
1329          bee         flies         <NA>              Grant,  Grant 1965
1330          bee         flies         <NA>              Grant,  Grant 1965
1331          bee         flies         <NA>              Grant,  Grant 1965
1332          bee         flies         <NA>              Grant,  Grant 1965
1333          bee         flies         <NA>              Grant,  Grant 1965
1334          bee         flies         <NA>              Grant,  Grant 1965
1335          bee         flies         <NA>              Grant,  Grant 1965
1336          bee         flies         <NA>              Grant,  Grant 1965
1337          bee         flies         <NA>              Grant,  Grant 1965
1338          bee         flies         <NA>              Grant,  Grant 1965
1339          bee         flies         <NA>              Grant,  Grant 1965
1340          bee         flies         <NA>              Grant,  Grant 1965
1341          bee         flies         <NA>              Grant,  Grant 1965
1342          bee         flies         <NA>              Grant,  Grant 1965
1343          bee         flies         <NA>              Grant,  Grant 1965
1344          bee         flies         <NA>              Grant,  Grant 1965
1345          bee         flies         <NA>              Grant,  Grant 1965
1346          bee         flies         <NA>              Grant,  Grant 1965
1347          bee         flies         <NA>              Grant,  Grant 1965
1348          bee         flies         <NA>              Grant,  Grant 1965
1349          bee         flies         <NA>              Grant,  Grant 1965
1350          bee         flies         <NA>              Grant,  Grant 1965
1351          bee         flies         <NA>              Grant,  Grant 1965
1352          bee         flies         <NA>              Grant,  Grant 1965
1353          bee         flies         <NA>              Grant,  Grant 1965
1354          bee         flies         <NA>              Grant,  Grant 1965
1355          bee         flies         <NA>              Grant,  Grant 1965
1356          bee         flies         <NA>              Grant,  Grant 1965
1357          bee         flies         <NA>              Grant,  Grant 1965
1358          bee         flies         <NA>              Grant,  Grant 1965
1359          bee         flies         <NA>              Grant,  Grant 1965
1360         <NA>          <NA>         <NA>              Grant,  Grant 1965
1361         <NA>          <NA>         <NA>              Grant,  Grant 1965
1362         <NA>          <NA>         <NA>              Grant,  Grant 1965
1363         <NA>          <NA>         <NA>              Grant,  Grant 1965
1364         <NA>          <NA>         <NA>              Grant,  Grant 1965
1365         <NA>          <NA>         <NA>              Grant,  Grant 1965
1366         <NA>          <NA>         <NA>              Grant,  Grant 1965
1367         <NA>          <NA>         <NA>              Grant,  Grant 1965
1368         <NA>          <NA>         <NA>              Grant,  Grant 1965
1369         <NA>          <NA>         <NA>              Grant,  Grant 1965
1370         <NA>          <NA>         <NA>              Grant,  Grant 1965
1371         <NA>          <NA>         <NA>              Grant,  Grant 1965
1372         <NA>          <NA>         <NA>              Grant,  Grant 1965
1373         <NA>          <NA>         <NA>              Grant,  Grant 1965
1374         <NA>          <NA>         <NA>              Grant,  Grant 1965
1375          bee          <NA>         <NA>              Grant,  Grant 1965
1376          bee          <NA>         <NA>              Grant,  Grant 1965
1377          bee          <NA>         <NA>              Grant,  Grant 1965
1378          bee          <NA>         <NA>              Grant,  Grant 1965
1379          bee          <NA>         <NA>              Grant,  Grant 1965
1380          bee          <NA>         <NA>              Grant,  Grant 1965
1381          bee          <NA>         <NA>              Grant,  Grant 1965
1382          bee          <NA>         <NA>              Grant,  Grant 1965
1383          bee          <NA>         <NA>              Grant,  Grant 1965
```
