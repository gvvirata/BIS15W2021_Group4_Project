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
2          Acanthogilia gloriosa                                     <NA>
3          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172802
4          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172801
5          Acanthogilia gloriosa       Acanthogilia gloriosa KANU00172802
6           Aliciella caespitosa          Aliciella caespitosa KSC0126229
7           Aliciella caespitosa          Aliciella caespitosa KSC0126229
8           Aliciella caespitosa                                     <NA>
9           Aliciella caespitosa          Aliciella caespitosa KSC0126229
10             Aliciella formosa                                     <NA>
11             Aliciella formosa               Aliciella formosa UNM75389
12             Aliciella formosa               Aliciella formosa MO275448
13             Aliciella formosa               Aliciella formosa UNM82540
14             Aliciella formosa               Aliciella formosa MO275448
15             Aliciella formosa               Aliciella formosa UNM75389
16             Aliciella formosa               Aliciella formosa MO275448
17             Aliciella formosa               Aliciella formosa UNM75389
18             Aliciella formosa               Aliciella formosa UNM82540
19             Aliciella formosa               Aliciella formosa UNM82540
20            Aliciella haydenii             Aliciella haydenii MO4390782
21            Aliciella haydenii             Aliciella haydenii MO4390782
22            Aliciella haydenii             Aliciella haydenii MO2469494
23            Aliciella haydenii             Aliciella haydenii MO4390782
24            Aliciella haydenii             Aliciella haydenii MO3495732
25            Aliciella haydenii              Aliciella haydenii MO247137
26            Aliciella haydenii                                     <NA>
27            Aliciella haydenii              Aliciella haydenii MO247137
28            Aliciella haydenii              Aliciella haydenii MO247137
29            Aliciella haydenii             Aliciella haydenii MO3495732
30            Aliciella haydenii             Aliciella haydenii RMH715060
31            Aliciella haydenii             Aliciella haydenii MO3495732
32            Aliciella haydenii             Aliciella haydenii RMH715060
33            Aliciella haydenii             Aliciella haydenii MO2469494
34            Aliciella haydenii             Aliciella haydenii RMH715060
35            Aliciella haydenii             Aliciella haydenii MO2469494
36         Aliciella heterostyla         Aliciella heterostyla KANU365799
37         Aliciella heterostyla         Aliciella heterostyla KANU363717
38         Aliciella heterostyla         Aliciella heterostyla KANU363717
39         Aliciella heterostyla                                     <NA>
40         Aliciella heterostyla         Aliciella heterostyla KANU365799
41         Aliciella heterostyla         Aliciella heterostyla KANU363717
42         Aliciella heterostyla         Aliciella heterostyla KANU365799
43      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00237643
44      Aliciella hutchinsifolia                                     <NA>
45      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00323745
46      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00237643
47      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00323745
48      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00237643
49      Aliciella hutchinsifolia    Aliciella hutchinsifolia KANU00323745
50           Aliciella latifolia         Aliciella latifolia KANU00237677
51           Aliciella latifolia         Aliciella latifolia KANU00237677
52           Aliciella latifolia         Aliciella latifolia KANU00237677
53           Aliciella latifolia        Aliciella latifolia KANU 00237676
54           Aliciella latifolia        Aliciella latifolia KANU 00237676
55           Aliciella latifolia         Aliciella latifolia KANU00237675
56           Aliciella latifolia         Aliciella latifolia KANU00237675
57           Aliciella latifolia         Aliciella latifolia KANU00237675
58           Aliciella latifolia                                     <NA>
59           Aliciella latifolia        Aliciella latifolia KANU 00237676
60          Aliciella leptomeria             Aliciella leptomeria UF39347
61          Aliciella leptomeria        Aliciella leptomeria KANU00237698
62          Aliciella leptomeria           Aliciella leptomeria FLAS39347
63          Aliciella leptomeria             Aliciella leptomeria UF39347
64          Aliciella leptomeria           Aliciella leptomeria FLAS39347
65          Aliciella leptomeria        Aliciella leptomeria KANU00237698
66          Aliciella leptomeria           Aliciella leptomeria FLAS39347
67          Aliciella leptomeria             Aliciella leptomeria UF39347
68          Aliciella leptomeria        Aliciella leptomeria KANU00237698
69          Aliciella leptomeria                                     <NA>
70             Aliciella lottiae                                     <NA>
71             Aliciella lottiae             Aliciella lottiae KANU365724
72             Aliciella lottiae             Aliciella lottiae KANU365724
73             Aliciella lottiae             Aliciella lottiae KANU365724
74          Aliciella mcvickerae          Aliciella mcvickerae KSC0126228
75          Aliciella mcvickerae          Aliciella mcvickerae KSC0126228
76          Aliciella mcvickerae                                     <NA>
77          Aliciella mcvickerae          Aliciella mcvickerae KSC0126228
78          Aliciella micromeria        Aliciella micromeria KANU00237702
79          Aliciella micromeria        Aliciella micromeria KANU00237702
80          Aliciella micromeria                                     <NA>
81          Aliciella micromeria        Aliciella micromeria KANU00237702
82           Aliciella monoensis            Aliciella monoensis MO5416709
83           Aliciella monoensis           Aliciella monoensis RSABG78468
84           Aliciella monoensis          Aliciella monoensis RSABG756276
85           Aliciella monoensis            Aliciella monoensis MO5416709
86           Aliciella monoensis            Aliciella monoensis MO3834767
87           Aliciella monoensis          Aliciella monoensis RSABG756276
88           Aliciella monoensis            Aliciella monoensis MO3834767
89           Aliciella monoensis                                     <NA>
90           Aliciella monoensis            Aliciella monoensis MO5416709
91           Aliciella monoensis          Aliciella monoensis RSABG756276
92           Aliciella monoensis           Aliciella monoensis RSABG78468
93           Aliciella monoensis           Aliciella monoensis RSABG78468
94           Aliciella monoensis            Aliciella monoensis MO3834767
95             Aliciella nyensis           Aliciella nyensis NYBG00964843
96             Aliciella nyensis           Aliciella nyensis NYBG00964843
97             Aliciella nyensis           Aliciella nyensis NYBG00964842
98             Aliciella nyensis           Aliciella nyensis NYBG00964845
99             Aliciella nyensis           Aliciella nyensis NYBG00964844
100            Aliciella nyensis           Aliciella nyensis NYBG00964844
101            Aliciella nyensis           Aliciella nyensis NYBG00964845
102            Aliciella nyensis           Aliciella nyensis NYBG00964842
103            Aliciella nyensis           Aliciella nyensis NYBG00964843
104            Aliciella nyensis           Aliciella nyensis NYBG00964845
105            Aliciella nyensis           Aliciella nyensis NYBG00964842
106            Aliciella nyensis                                     <NA>
107            Aliciella nyensis           Aliciella nyensis NYBG00964844
108        Aliciella pinnatifida       Aliciella pinnatifida KANU00237715
109        Aliciella pinnatifida         Aliciella pinnatifida KSC0126336
110        Aliciella pinnatifida       Aliciella pinnatifida KANU00237715
111        Aliciella pinnatifida                                     <NA>
112        Aliciella pinnatifida       Aliciella pinnatifida KANU00237717
113        Aliciella pinnatifida       Aliciella pinnatifida KANU00237715
114        Aliciella pinnatifida       Aliciella pinnatifida KANU00237717
115        Aliciella pinnatifida         Aliciella pinnatifida KSC0126336
116        Aliciella pinnatifida       Aliciella pinnatifida KANU00237717
117        Aliciella pinnatifida         Aliciella pinnatifida KSC0126336
118            Aliciella ripleyi            Aliciella ripleyi RSABG311225
119            Aliciella ripleyi            Aliciella ripleyi RSABG678140
120            Aliciella ripleyi            Aliciella ripleyi RSABG678140
121            Aliciella ripleyi                                     <NA>
122            Aliciella ripleyi            Aliciella ripleyi RSABG311225
123            Aliciella ripleyi            Aliciella ripleyi RSABG311225
124            Aliciella ripleyi            Aliciella ripleyi RSABG678140
125        Aliciella stenothyrsa       Aliciella stenothyrsa KANU00324716
126        Aliciella stenothyrsa         Aliciella stenothyrsa KSC0126391
127        Aliciella stenothyrsa       Aliciella stenothyrsa KANU00324699
128        Aliciella stenothyrsa       Aliciella stenothyrsa KANU00324716
129        Aliciella stenothyrsa         Aliciella stenothyrsa KSC0126391
130        Aliciella stenothyrsa       Aliciella stenothyrsa KANU00324716
131        Aliciella stenothyrsa         Aliciella stenothyrsa KSC0126391
132        Aliciella stenothyrsa       Aliciella stenothyrsa KANU00324699
133        Aliciella stenothyrsa                                     <NA>
134        Aliciella stenothyrsa       Aliciella stenothyrsa KANU00324699
135         Aliciella subacaulis         Aliciella subacaulis RSABG671709
136         Aliciella subacaulis         Aliciella subacaulis RSABG767897
137         Aliciella subacaulis         Aliciella subacaulis RSABG767897
138         Aliciella subacaulis         Aliciella subacaulis RSABG671709
139         Aliciella subacaulis                                     <NA>
140         Aliciella subacaulis         Aliciella subacaulis RSABG671709
141         Aliciella subacaulis         Aliciella subacaulis RSABG767897
142            Aliciella subnuda           Aliciella subnuda KANU00237798
143            Aliciella subnuda           Aliciella subnuda KANU00237796
144            Aliciella subnuda           Aliciella subnuda KANU00237795
145            Aliciella subnuda           Aliciella subnuda KANU00237795
146            Aliciella subnuda           Aliciella subnuda KANU00247797
147            Aliciella subnuda             Aliciella subnuda KANU362789
148            Aliciella subnuda           Aliciella subnuda KANU00237798
149            Aliciella subnuda           Aliciella subnuda KANU00247797
150            Aliciella subnuda             Aliciella subnuda KSC0126390
151            Aliciella subnuda           Aliciella subnuda KANU00237795
152            Aliciella subnuda                                     <NA>
153            Aliciella subnuda           Aliciella subnuda KANU00237796
154            Aliciella subnuda             Aliciella subnuda KSC0126390
155            Aliciella subnuda           Aliciella subnuda KANU00237796
156            Aliciella subnuda             Aliciella subnuda KSC0126390
157            Aliciella subnuda           Aliciella subnuda KANU00247797
158            Aliciella subnuda             Aliciella subnuda KANU362789
159            Aliciella subnuda           Aliciella subnuda KANU00237798
160            Aliciella subnuda             Aliciella subnuda KANU362789
161             Aliciella tenuis                Aliciella tenuis UF190756
162             Aliciella tenuis                Aliciella tenuis UF190756
163             Aliciella tenuis                                     <NA>
164             Aliciella tenuis                Aliciella tenuis UF190756
165            Aliciella triodon              Aliciella triodon MO1037260
166            Aliciella triodon              Aliciella triodon MO4050306
167            Aliciella triodon              Aliciella triodon MO1037260
168            Aliciella triodon              Aliciella triodon MO4050306
169            Aliciella triodon              Aliciella triodon MO1037260
170            Aliciella triodon              Aliciella triodon MO2753602
171            Aliciella triodon              Aliciella triodon MO2753602
172            Aliciella triodon              Aliciella triodon MO2753602
173            Aliciella triodon                                     <NA>
174      Allophyllum divaricatum       Allophyllum divaricatum KSC0124636
175      Allophyllum divaricatum                                     <NA>
176      Allophyllum divaricatum       Allophyllum divaricatum KSC0124636
177      Allophyllum divaricatum       Allophyllum divaricatum KSC0124636
178        Allophyllum gilioides         Allophyllum gilioides FLAS119038
179        Allophyllum gilioides         Allophyllum gilioides FLAS124817
180        Allophyllum gilioides         Allophyllum gilioides FLAS119038
181        Allophyllum gilioides         Allophyllum gilioides KSC0126262
182        Allophyllum gilioides         Allophyllum gilioides FLAS119038
183        Allophyllum gilioides         Allophyllum gilioides KSC0126266
184        Allophyllum gilioides         Allophyllum gilioides KSC0126266
185        Allophyllum gilioides         Allophyllum gilioides KSC0126262
186        Allophyllum gilioides          Allophyllum gilioides FLAS72303
187        Allophyllum gilioides         Allophyllum gilioides KSC0126261
188        Allophyllum gilioides       Allophyllum gilioides KANU00176464
189        Allophyllum gilioides       Allophyllum gilioides KANU00176463
190        Allophyllum gilioides         Allophyllum gilioides FLAS124817
191        Allophyllum gilioides         Allophyllum gilioides KSC0126262
192        Allophyllum gilioides         Allophyllum gilioides KSC0126266
193        Allophyllum gilioides         Allophyllum gilioides FLAS124817
194        Allophyllum gilioides       Allophyllum gilioides KANU00176463
195        Allophyllum gilioides          Allophyllum gilioides FLAS72302
196        Allophyllum gilioides       Allophyllum gilioides KANU00176464
197        Allophyllum gilioides       Allophyllum gilioides KANU00176464
198        Allophyllum gilioides          Allophyllum gilioides FLAS72303
199        Allophyllum gilioides         Allophyllum gilioides KSC0126261
200        Allophyllum gilioides          Allophyllum gilioides FLAS79454
201        Allophyllum gilioides          Allophyllum gilioides FLAS72302
202        Allophyllum gilioides          Allophyllum gilioides FLAS72302
203        Allophyllum gilioides          Allophyllum gilioides FLAS79454
204        Allophyllum gilioides         Allophyllum gilioides KSC0126261
205        Allophyllum gilioides          Allophyllum gilioides FLAS72303
206        Allophyllum gilioides       Allophyllum gilioides KANU00176463
207        Allophyllum gilioides          Allophyllum gilioides FLAS79454
208       Allophyllum glutinosum                                     <NA>
209       Allophyllum glutinosum        Allophyllum glutinosum KSC0126064
210       Allophyllum glutinosum       Allophyllum glutinosum KANU0176465
211       Allophyllum glutinosum       Allophyllum glutinosum KANU0176465
212       Allophyllum glutinosum        Allophyllum glutinosum KSC0126064
213       Allophyllum glutinosum       Allophyllum glutinosum KANU0176465
214       Allophyllum glutinosum        Allophyllum glutinosum KSC0126064
215    Allophyllum integrifolium      Allophyllum integrifolium MO4390038
216    Allophyllum integrifolium      Allophyllum integrifolium MO4270063
217    Allophyllum integrifolium      Allophyllum integrifolium MO4270063
218    Allophyllum integrifolium      Allophyllum integrifolium MO4390038
219    Allophyllum integrifolium      Allophyllum integrifolium MO3108259
220    Allophyllum integrifolium      Allophyllum integrifolium MO3108259
221    Allophyllum integrifolium                                     <NA>
222    Allophyllum integrifolium      Allophyllum integrifolium MO4390038
223    Allophyllum integrifolium      Allophyllum integrifolium MO4270063
224    Allophyllum integrifolium      Allophyllum integrifolium MO3108259
225        Allophyllum violaceum            Allophyllum violaceum 3108257
226        Allophyllum violaceum            Allophyllum violaceum 3024210
227        Allophyllum violaceum            Allophyllum violaceum 3108257
228        Allophyllum violaceum            Allophyllum violaceum 4322220
229        Allophyllum violaceum            Allophyllum violaceum 3108257
230        Allophyllum violaceum            Allophyllum violaceum 4322220
231        Allophyllum violaceum            Allophyllum violaceum 4322220
232        Allophyllum violaceum            Allophyllum violaceum 3024210
233        Allophyllum violaceum            Allophyllum violaceum 3024210
234        Allophyllum violaceum                                     <NA>
235       Bonplandia geminiflora         Bonplandia geminiflora MO2606326
236       Bonplandia geminiflora        Bonplandia geminiflora MO04668787
237       Bonplandia geminiflora         Bonplandia geminiflora MO2606326
238       Bonplandia geminiflora                                     <NA>
239       Bonplandia geminiflora        Bonplandia geminiflora MO04668787
240       Bonplandia geminiflora        Bonplandia geminiflora MO04668787
241       Bonplandia geminiflora         Bonplandia geminiflora MO2606326
242          Bryantiella palmeri             Bryantiella palmeri UF106211
243          Bryantiella palmeri             Bryantiella palmeri UF106211
244          Bryantiella palmeri             Bryantiella palmeri UF106211
245          Bryantiella palmeri                                     <NA>
246             Cantua buxifolia               Cantua buxifolia MO1284774
247             Cantua buxifolia               Cantua buxifolia MO6314998
248             Cantua buxifolia               Cantua buxifolia MO5782510
249             Cantua buxifolia                                     <NA>
250             Cantua buxifolia               Cantua buxifolia MO6314998
251             Cantua buxifolia               Cantua buxifolia MO5782510
252             Cantua buxifolia               Cantua buxifolia MO5782510
253             Cantua buxifolia               Cantua buxifolia MO1284774
254             Cantua buxifolia               Cantua buxifolia MO6314998
255             Cantua buxifolia               Cantua buxifolia MO1284774
256            Cantua candelilla               Cantua candelilla MO587382
257            Cantua candelilla                                     <NA>
258            Cantua candelilla               Cantua candelilla MO587382
259            Cantua candelilla              Cantua candelilla MO6197258
260            Cantua candelilla              Cantua candelilla MO6197258
261            Cantua candelilla               Cantua candelilla MO587382
262            Cantua candelilla              Cantua candelilla MO6197258
263            Cantua cuzcoensis            Cantua cuzcoensis RSABG670098
264            Cantua cuzcoensis                                     <NA>
265            Cantua cuzcoensis            Cantua cuzcoensis RSABG670098
266            Cantua cuzcoensis            Cantua cuzcoensis RSABG670098
267            Cantua dendritica              Cantua dendritica MO6128945
268            Cantua dendritica              Cantua dendritica MO6128945
269            Cantua dendritica                                     <NA>
270            Cantua dendritica              Cantua dendritica MO6128945
271              Cantua flexuosa                Cantua flexuosa MO6314697
272              Cantua flexuosa                                     <NA>
273              Cantua flexuosa                Cantua flexuosa MO6122610
274              Cantua flexuosa                Cantua flexuosa MO6314697
275              Cantua flexuosa                Cantua flexuosa MO6122610
276              Cantua flexuosa                Cantua flexuosa MO3590670
277              Cantua flexuosa                Cantua flexuosa MO6314697
278              Cantua flexuosa                Cantua flexuosa MO3590670
279              Cantua flexuosa                Cantua flexuosa MO6122610
280              Cantua flexuosa                Cantua flexuosa MO3590670
281             Cantua pyrifolia               Cantua pyrifolia MO5708217
282             Cantua pyrifolia               Cantua pyrifolia MO6052283
283             Cantua pyrifolia               Cantua pyrifolia MO5782517
284             Cantua pyrifolia               Cantua pyrifolia MO5708217
285             Cantua pyrifolia               Cantua pyrifolia MO6052283
286             Cantua pyrifolia               Cantua pyrifolia MO5782517
287             Cantua pyrifolia               Cantua pyrifolia MO5708217
288             Cantua pyrifolia                                     <NA>
289             Cantua pyrifolia               Cantua pyrifolia MO6052283
290             Cantua pyrifolia               Cantua pyrifolia MO5782517
291           Cantua quercifolia             Cantua quercifolia MO5782518
292           Cantua quercifolia             Cantua quercifolia MO5782509
293           Cantua quercifolia             Cantua quercifolia MO5782509
294           Cantua quercifolia                                     <NA>
295           Cantua quercifolia             Cantua quercifolia MO5782509
296           Cantua quercifolia             Cantua quercifolia MO5782518
297           Cantua quercifolia             Cantua quercifolia MO5755785
298           Cantua quercifolia             Cantua quercifolia MO5782518
299           Cantua quercifolia             Cantua quercifolia MO6030150
300           Cantua quercifolia             Cantua quercifolia MO5755785
301           Cantua quercifolia             Cantua quercifolia MO6030150
302           Cantua quercifolia             Cantua quercifolia MO6030150
303           Cantua quercifolia             Cantua quercifolia MO5755785
304             Cantua volcanica               Cantua volcanica MO5256597
305             Cantua volcanica               Cantua volcanica MO5256597
306             Cantua volcanica               Cantua volcanica MO5256597
307             Cantua volcanica                                     <NA>
308        Cobaea aequatoriensis        Cobaea aequatoriensis RSABG717593
309         Cobaea aschersoniana           Cobaea aschersoniana MO3602165
310              Cobaea biaurita                Cobaea biaurita MO3861020
311              Cobaea biaurita                Cobaea biaurita MO3153366
312              Cobaea biaurita                Cobaea biaurita MO3153366
313              Cobaea biaurita                Cobaea biaurita MO3281095
314              Cobaea biaurita                                     <NA>
315              Cobaea biaurita                Cobaea biaurita MO3761514
316              Cobaea biaurita                Cobaea biaurita MO3602168
317              Cobaea biaurita                Cobaea biaurita MO3281095
318              Cobaea biaurita                Cobaea biaurita MO3281095
319           Cobaea campanulata                                     <NA>
320           Cobaea campanulata           Cobaea campanulata RSABG697003
321              Cobaea gracilis                Cobaea gracilis MO1146113
322              Cobaea gracilis                Cobaea gracilis MO2988813
323              Cobaea gracilis                Cobaea gracilis MO1111444
324              Cobaea gracilis                Cobaea gracilis MO1146113
325              Cobaea gracilis                Cobaea gracilis MO1111444
326              Cobaea gracilis                Cobaea gracilis MO1111444
327              Cobaea gracilis                Cobaea gracilis MO6207054
328              Cobaea gracilis                                     <NA>
329                 Cobaea lutea                        Cobaea lutea NYBG
330                 Cobaea lutea                      Cobaea lutea NYBG 7
331                 Cobaea lutea                                     <NA>
332                 Cobaea lutea                   Cobaea lutea MO5897922
333                 Cobaea lutea                   Cobaea lutea MO5897721
334                 Cobaea lutea                   Cobaea lutea MO6058036
335                 Cobaea lutea                      Cobaea lutea NYBG 5
336                 Cobaea lutea                   Cobaea lutea MO5897921
337                 Cobaea lutea                   Cobaea lutea MO5871652
338                 Cobaea lutea                        Cobaea lutea NYBG
339                 Cobaea lutea                      Cobaea lutea NYBG 6
340                 Cobaea lutea                      Cobaea lutea NYBG 2
341                 Cobaea lutea                      Cobaea lutea NYBG 3
342                 Cobaea minor                   Cobaea minor MO3861025
343                 Cobaea minor                   Cobaea minor MO3861033
344                 Cobaea minor                   Cobaea minor MO3861029
345                 Cobaea minor                   Cobaea minor MO1688701
346                 Cobaea minor                                     <NA>
347           Cobaea pachysepala                                     <NA>
348           Cobaea pachysepala             Cobaea pachysepala MO3602161
349           Cobaea pachysepala             Cobaea pachysepala MO3602164
350           Cobaea pachysepala             Cobaea pachysepala MO1278345
351           Cobaea pachysepala             Cobaea pachysepala MO5706100
352           Cobaea pachysepala             Cobaea pachysepala MO3281097
353           Cobaea pachysepala             Cobaea pachysepala MO3281097
354          Cobaea penduliflora                                     <NA>
355          Cobaea penduliflora            Cobaea penduliflora MO2724237
356          Cobaea penduliflora             Cobaea penduliflora MO113696
357              Cobaea pringlei             Cobaea pringlei KANU00218685
358          Cobaea rotundiflora                                     <NA>
359          Cobaea rotundiflora            Cobaea rotundiflora MO3750483
360          Cobaea rotundiflora            Cobaea rotundiflora MO3750483
361              Cobaea scandens                Cobaea scandens FLAS87478
362              Cobaea scandens                Cobaea scandens FLAS87478
363              Cobaea scandens               Cobaea scandens FLAS135739
364              Cobaea scandens                Cobaea scandens FLAS87478
365              Cobaea scandens               Cobaea scandens FLAS135739
366              Cobaea scandens                                     <NA>
367              Cobaea scandens               Cobaea scandens FLAS221362
368            Cobaea stipularis              Cobaea stipularis MO3602162
369            Cobaea stipularis                                     <NA>
370            Cobaea stipularis              Cobaea stipularis MO5706106
371               Cobaea trianae                 Cobaea trianae MO6318229
372               Cobaea trianae                 Cobaea trianae MO3857537
373               Cobaea trianae                 Cobaea trianae MO6318229
374               Cobaea trianae                 Cobaea trianae MO3612189
375             Collomia biflora               Collomia biflora MO1241840
376             Collomia biflora               Collomia biflora MO2387056
377             Collomia biflora               Collomia biflora MO1241840
378             Collomia biflora               Collomia biflora MO2387056
379             Collomia biflora               Collomia biflora MO4005361
380             Collomia biflora               Collomia biflora MO4005361
381             Collomia biflora               Collomia biflora MO4005361
382             Collomia biflora               Collomia biflora MO1241840
383             Collomia biflora                                     <NA>
384             Collomia biflora               Collomia biflora MO2387056
385             Collomia debilis             Collomia debilis KANU0021885
386             Collomia debilis              Collomia debilis KSC0126238
387             Collomia debilis            Collomia debilis KANU00218887
388             Collomia debilis              Collomia debilis KSC0126238
389             Collomia debilis            Collomia debilis KANU00218887
390             Collomia debilis            Collomia debilis KANU00218886
391             Collomia debilis                                     <NA>
392             Collomia debilis              Collomia debilis KSC0126238
393             Collomia debilis            Collomia debilis KANU00218887
394             Collomia debilis            Collomia debilis KANU00218886
395             Collomia debilis             Collomia debilis KANU0021885
396        Collomia diversifolia       Collomia diversifolia KANU00218888
397        Collomia diversifolia                                     <NA>
398        Collomia diversifolia       Collomia diversifolia KANU00218888
399        Collomia diversifolia       Collomia diversifolia KANU00218888
400         Collomia grandiflora          Collomia grandiflora KSC0126124
401         Collomia grandiflora          Collomia grandiflora KANU363773
402         Collomia grandiflora           Collomia grandiflora FLAS53043
403         Collomia grandiflora           Collomia grandiflora FLAS53064
404         Collomia grandiflora           Collomia grandiflora FLAS53043
405         Collomia grandiflora          Collomia grandiflora KSC0126124
406         Collomia grandiflora          Collomia grandiflora FLAS124816
407         Collomia grandiflora           Collomia grandiflora FLAS53043
408         Collomia grandiflora          Collomia grandiflora KANU363773
409         Collomia grandiflora          Collomia grandiflora KANU363773
410         Collomia grandiflora           Collomia grandiflora FLAS55664
411         Collomia grandiflora           Collomia grandiflora FLAS55664
412         Collomia grandiflora           Collomia grandiflora FLAS53064
413         Collomia grandiflora          Collomia grandiflora KSC0126126
414         Collomia grandiflora          Collomia grandiflora KSC0126126
415         Collomia grandiflora           Collomia grandiflora FLAS53064
416         Collomia grandiflora                                     <NA>
417         Collomia grandiflora        Collomia grandiflora KANU00218896
418         Collomia grandiflora        Collomia grandiflora KANU00218896
419         Collomia grandiflora           Collomia grandiflora FLAS55664
420         Collomia grandiflora          Collomia grandiflora KSC0126124
421         Collomia grandiflora          Collomia grandiflora KSC0126125
422         Collomia grandiflora          Collomia grandiflora KSC0126125
423         Collomia grandiflora          Collomia grandiflora KSC0126125
424         Collomia grandiflora          Collomia grandiflora KSC0126126
425         Collomia grandiflora        Collomia grandiflora KANU00218896
426        Collomia heterophylla           Collomia heterophylla FLAS5666
427        Collomia heterophylla       Collomia heterophylla KANU00218904
428        Collomia heterophylla         Collomia heterophylla KSC0126123
429        Collomia heterophylla       Collomia heterophylla KANU00218904
430        Collomia heterophylla       Collomia heterophylla KANU00218899
431        Collomia heterophylla          Collomia heterophylla FLAS53044
432        Collomia heterophylla       Collomia heterophylla KANU00218899
433        Collomia heterophylla       Collomia heterophylla KANU00218902
434        Collomia heterophylla           Collomia heterophylla FLAS5666
435        Collomia heterophylla       Collomia heterophylla KANU00218899
436        Collomia heterophylla       Collomia heterophylla KANU00218904
437        Collomia heterophylla           Collomia heterophylla FLAS5666
438        Collomia heterophylla          Collomia heterophylla FLAS72212
439        Collomia heterophylla          Collomia heterophylla FLAS55665
440        Collomia heterophylla          Collomia heterophylla FLAS53044
441        Collomia heterophylla          Collomia heterophylla FLAS72212
442        Collomia heterophylla         Collomia heterophylla KSC0126127
443        Collomia heterophylla       Collomia heterophylla KANU00218902
444        Collomia heterophylla          Collomia heterophylla FLAS72212
445        Collomia heterophylla          Collomia heterophylla FLAS55665
446        Collomia heterophylla         Collomia heterophylla KSC0126128
447        Collomia heterophylla          Collomia heterophylla FLAS55665
448        Collomia heterophylla       Collomia heterophylla KANU00218902
449        Collomia heterophylla         Collomia heterophylla FLAS118998
450        Collomia heterophylla         Collomia heterophylla KSC0126128
451        Collomia heterophylla         Collomia heterophylla KSC0126123
452        Collomia heterophylla          Collomia heterophylla FLAS53179
453        Collomia heterophylla         Collomia heterophylla KSC0126127
454        Collomia heterophylla                                     <NA>
455        Collomia heterophylla          Collomia heterophylla FLAS53179
456        Collomia heterophylla         Collomia heterophylla KSC0126123
457        Collomia heterophylla          Collomia heterophylla FLAS53179
458        Collomia heterophylla         Collomia heterophylla KSC0126128
459        Collomia heterophylla         Collomia heterophylla FLAS118998
460        Collomia heterophylla          Collomia heterophylla FLAS53044
461        Collomia heterophylla          Collomia heterophylla FLAS53301
462        Collomia heterophylla         Collomia heterophylla KSC0126127
463        Collomia heterophylla          Collomia heterophylla FLAS53301
464        Collomia heterophylla         Collomia heterophylla FLAS118998
465            Collomia linearis           Collomia linearis KANU00326484
466            Collomia linearis           Collomia linearis KANU00326484
467            Collomia linearis              Collomia linearis FLAS57177
468            Collomia linearis             Collomia linearis FLAS190754
469            Collomia linearis             Collomia linearis KSC0126151
470            Collomia linearis              Collomia linearis FLAS20300
471            Collomia linearis           Collomia linearis KANU00326484
472            Collomia linearis             Collomia linearis FLAS190754
473            Collomia linearis             Collomia linearis KSC0126156
474            Collomia linearis              Collomia linearis FLAS44045
475            Collomia linearis              Collomia linearis FLAS40535
476            Collomia linearis              Collomia linearis FLAS40535
477            Collomia linearis             Collomia linearis KSC0126154
478            Collomia linearis           Collomia linearis KANU00218981
479            Collomia linearis              Collomia linearis FLAS57177
480            Collomia linearis             Collomia linearis FLAS147432
481            Collomia linearis              Collomia linearis FLAS40535
482            Collomia linearis           Collomia linearis KANU00218987
483            Collomia linearis             Collomia linearis KSC0126151
484            Collomia linearis              Collomia linearis FLAS20300
485            Collomia linearis           Collomia linearis KANU00218987
486            Collomia linearis             Collomia linearis FLAS147432
487            Collomia linearis           Collomia linearis KANU00218981
488            Collomia linearis             Collomia linearis FLAS190754
489            Collomia linearis             Collomia linearis FLAS122937
490            Collomia linearis              Collomia linearis FLAS20300
491            Collomia linearis           Collomia linearis KANU00218987
492            Collomia linearis             Collomia linearis FLAS122937
493            Collomia linearis              Collomia linearis FLAS44045
494            Collomia linearis             Collomia linearis KSC0126156
495            Collomia linearis             Collomia linearis KSC0126156
496            Collomia linearis           Collomia linearis KANU00218981
497            Collomia linearis              Collomia linearis FLAS57177
498            Collomia linearis             Collomia linearis KSC0126154
499            Collomia linearis             Collomia linearis KSC0126154
500            Collomia linearis              Collomia linearis FLAS44045
501            Collomia linearis             Collomia linearis FLAS147432
502            Collomia linearis             Collomia linearis FLAS122937
503            Collomia linearis                                     <NA>
504            Collomia linearis             Collomia linearis KSC0126151
505              Collomia mazama                Collomia mazama MO5416813
506              Collomia mazama                Collomia mazama MO1285240
507              Collomia mazama                Collomia mazama MO1285240
508              Collomia mazama                Collomia mazama MO5416813
509              Collomia mazama                Collomia mazama MO5416813
510              Collomia mazama                                     <NA>
511              Collomia mazama                Collomia mazama MO1285240
512          Collomia rawsoniana             Collomia rawsoniana MO933189
513          Collomia rawsoniana             Collomia rawsoniana MO933189
514          Collomia rawsoniana            Collomia rawsoniana MO2754561
515          Collomia rawsoniana                                     <NA>
516          Collomia rawsoniana             Collomia rawsoniana MO933189
517          Collomia rawsoniana             Collomia rawsoniana MO119646
518          Collomia rawsoniana             Collomia rawsoniana MO119646
519          Collomia rawsoniana            Collomia rawsoniana MO2754561
520          Collomia rawsoniana             Collomia rawsoniana MO119646
521          Collomia rawsoniana            Collomia rawsoniana MO2754561
522             Collomia tenella               Collomia tenella MO1008736
523             Collomia tenella               Collomia tenella MO1046558
524             Collomia tenella               Collomia tenella MO1008736
525             Collomia tenella               Collomia tenella FLAS61179
526             Collomia tenella                                     <NA>
527             Collomia tenella               Collomia tenella MO1046558
528             Collomia tenella               Collomia tenella FLAS61179
529             Collomia tenella               Collomia tenella MO1081740
530             Collomia tenella               Collomia tenella MO1046558
531             Collomia tenella               Collomia tenella MO1008736
532             Collomia tenella               Collomia tenella MO1081740
533             Collomia tenella               Collomia tenella MO1081740
534             Collomia tenella               Collomia tenella FLAS61179
535           Collomia tinctoria            Collomia tinctoria KANU360011
536           Collomia tinctoria            Collomia tinctoria KANU360011
537           Collomia tinctoria            Collomia tinctoria KANU360011
538           Collomia tinctoria            Collomia tinctoria KANU367450
539           Collomia tinctoria          Collomia tinctoria KANU00219029
540           Collomia tinctoria            Collomia tinctoria KANU367450
541           Collomia tinctoria          Collomia tinctoria KANU00219029
542           Collomia tinctoria                                     <NA>
543           Collomia tinctoria          Collomia tinctoria KANU00219029
544           Collomia tinctoria            Collomia tinctoria KANU367450
545              Collomia tracyi                                     <NA>
546              Collomia tracyi                Collomia tracyi MO3248515
547              Collomia tracyi                Collomia tracyi MO2753241
548              Collomia tracyi                Collomia tracyi MO2753241
549              Collomia tracyi                Collomia tracyi MO3248515
550              Collomia tracyi                Collomia tracyi MO2753241
551              Collomia tracyi                Collomia tracyi MO1704173
552              Collomia tracyi                Collomia tracyi MO3248515
553              Collomia tracyi                Collomia tracyi MO1704173
554                Dayia grantii                                     <NA>
555                Dayia grantii                Dayia grantii RSABG672123
556                Dayia grantii                Dayia grantii RSABG672123
557                 Dayia scabra                 Dayia scabra RSABG703018
558                 Dayia scabra                 Dayia scabra RSABG659006
559                 Dayia scabra                 Dayia scabra RSABG703018
560                 Dayia scabra                 Dayia scabra RSABG659006
561                 Dayia scabra                 Dayia scabra RSABG659006
562                 Dayia scabra                 Dayia scabra RSABG703018
563                 Dayia scabra                                     <NA>
564           Eriastrum abramsii             Eriastrum abramsii MO1242971
565           Eriastrum abramsii             Eriastrum abramsii MO1242971
566           Eriastrum abramsii              Eriastrum abramsii MO997258
567           Eriastrum abramsii              Eriastrum abramsii MO997258
568           Eriastrum abramsii             Eriastrum abramsii MO1242971
569           Eriastrum abramsii             Eriastrum abramsii MO1231957
570           Eriastrum abramsii              Eriastrum abramsii MO997258
571           Eriastrum abramsii                                     <NA>
572        Eriastrum densifolium         Eriastrum densifolium KSC0126183
573        Eriastrum densifolium         Eriastrum densifolium KSC0126235
574        Eriastrum densifolium       Eriastrum densifolium KANU00229151
575        Eriastrum densifolium       Eriastrum densifolium KANU00229151
576        Eriastrum densifolium         Eriastrum densifolium KSC0126236
577        Eriastrum densifolium          Eriastrum densifolium FLAS40522
578        Eriastrum densifolium       Eriastrum densifolium KANU00229153
579        Eriastrum densifolium       Eriastrum densifolium KANU00229150
580        Eriastrum densifolium         Eriastrum densifolium KSC0126235
581        Eriastrum densifolium       Eriastrum densifolium KANU00229150
582        Eriastrum densifolium       Eriastrum densifolium KANU00229151
583        Eriastrum densifolium         Eriastrum densifolium KSC0126236
584        Eriastrum densifolium         Eriastrum densifolium KSC0126235
585        Eriastrum densifolium       Eriastrum densifolium KANU00229153
586        Eriastrum densifolium                                     <NA>
587        Eriastrum densifolium         Eriastrum densifolium KSC0126183
588        Eriastrum densifolium       Eriastrum densifolium KANU00229153
589        Eriastrum densifolium         Eriastrum densifolium KSC0126183
590        Eriastrum densifolium          Eriastrum densifolium FLAS40522
591        Eriastrum densifolium         Eriastrum densifolium KSC0126236
592        Eriastrum densifolium       Eriastrum densifolium KANU00229150
593        Eriastrum densifolium          Eriastrum densifolium FLAS40522
594           Eriastrum diffusum                                     <NA>
595           Eriastrum diffusum          Eriastrum diffusum KANU00229155
596           Eriastrum diffusum            Eriastrum diffusum KANU364546
597           Eriastrum diffusum            Eriastrum diffusum KSC0126180
598           Eriastrum diffusum          Eriastrum diffusum KANU00229155
599           Eriastrum diffusum            Eriastrum diffusum KANU364546
600           Eriastrum diffusum            Eriastrum diffusum KSC0126180
601           Eriastrum diffusum            Eriastrum diffusum KANU364546
602           Eriastrum diffusum          Eriastrum diffusum KANU00229155
603           Eriastrum diffusum            Eriastrum diffusum KSC0126180
604           Eriastrum eremicum            Eriastrum eremicum KSC0126181
605           Eriastrum eremicum            Eriastrum eremicum KANI361667
606           Eriastrum eremicum            Eriastrum eremicum FLAS158631
607           Eriastrum eremicum            Eriastrum eremicum KSC0126182
608           Eriastrum eremicum            Eriastrum eremicum KANI361667
609           Eriastrum eremicum            Eriastrum eremicum KANI361667
610           Eriastrum eremicum            Eriastrum eremicum KSC0126182
611           Eriastrum eremicum            Eriastrum eremicum KSC0126182
612           Eriastrum eremicum            Eriastrum eremicum FLAS158631
613           Eriastrum eremicum            Eriastrum eremicum KSC0126274
614           Eriastrum eremicum          Eriastrum eremicum KANU00229159
615           Eriastrum eremicum          Eriastrum eremicum KANU00229158
616           Eriastrum eremicum            Eriastrum eremicum FLAS158631
617           Eriastrum eremicum            Eriastrum eremicum KSC0126274
618           Eriastrum eremicum                                     <NA>
619           Eriastrum eremicum            Eriastrum eremicum KSC0126274
620           Eriastrum eremicum          Eriastrum eremicum KANU00229159
621           Eriastrum eremicum          Eriastrum eremicum KANU00229158
622           Eriastrum eremicum          Eriastrum eremicum KANU00229158
623           Eriastrum eremicum            Eriastrum eremicum KSC0126181
624           Eriastrum eremicum          Eriastrum eremicum KANU00229159
625           Eriastrum eremicum            Eriastrum eremicum KSC0126181
626          Eriastrum harwoodii          Eriastrum harwoodii RSABG781391
627          Eriastrum harwoodii                                     <NA>
628          Eriastrum harwoodii          Eriastrum harwoodii RSABG781391
629            Eriastrum hooveri            Eriastrum hooveri RSABG359916
630            Eriastrum hooveri            Eriastrum hooveri RSABG730294
631            Eriastrum hooveri            Eriastrum hooveri RSABG730294
632            Eriastrum hooveri            Eriastrum hooveri RSABG359916
633            Eriastrum hooveri            Eriastrum hooveri RSABG730294
634            Eriastrum hooveri                                     <NA>
635            Eriastrum hooveri            Eriastrum hooveri RSABG359916
636             Eriastrum luteum               Eriastrum luteum MO2753702
637             Eriastrum luteum                                     <NA>
638             Eriastrum luteum               Eriastrum luteum MO2753700
639             Eriastrum luteum             Eriastrum luteum RSABG523184
640             Eriastrum luteum             Eriastrum luteum RSABG523184
641             Eriastrum luteum               Eriastrum luteum MO2753700
642             Eriastrum luteum             Eriastrum luteum RSABG523184
643             Eriastrum luteum               Eriastrum luteum MO2753700
644        Eriastrum pluriflorum         Eriastrum pluriflorum KSC0126340
645        Eriastrum pluriflorum        Eriastrum plurifloum KANU00229169
646        Eriastrum pluriflorum         Eriastrum pluriflorum KSC0126340
647        Eriastrum pluriflorum                                     <NA>
648        Eriastrum pluriflorum         Eriastrum pluriflorum KSC0126341
649        Eriastrum pluriflorum         Eriastrum pluriflorum KSC0126341
650        Eriastrum pluriflorum       Eriastrum pluriflorum KANU00229170
651        Eriastrum pluriflorum       Eriastrum pluriflorum KANU00229170
652        Eriastrum pluriflorum         Eriastrum pluriflorum KSC0126341
653        Eriastrum pluriflorum       Eriastrum pluriflorum KANU00229170
654        Eriastrum pluriflorum         Eriastrum pluriflorum KSC0126340
655        Eriastrum pluriflorum       Eriastrum pluriflorum KANU00229169
656        Eriastrum pluriflorum       Eriastrum pluriflorum KANU00229169
657        Eriastrum sapphirinum       Eriastrum sapphirinum KANU00229171
658        Eriastrum sapphirinum                                     <NA>
659        Eriastrum sapphirinum         Eriastrum sapphirinum FLAS124728
660        Eriastrum sapphirinum         Eriastrum sapphirinum FLAS124728
661        Eriastrum sapphirinum         Eriastrum sapphirinum FLAS124728
662        Eriastrum sapphirinum       Eriastrum sapphirinum KANU00229171
663        Eriastrum sapphirinum       Eriastrum sapphirinum KANU00229171
664           Eriastrum signatum             Eriastrum signatum MO5416786
665           Eriastrum signatum                                     <NA>
666           Eriastrum signatum             Eriastrum signatum MO5416786
667           Eriastrum signatum             Eriastrum signatum MO5416786
668       Eriastrum sparsiflorum                                     <NA>
669       Eriastrum sparsiflorum      Eriastrum sparsiflorum KANU00229173
670       Eriastrum sparsiflorum      Eriastrum sparsiflorum KANU00329996
671       Eriastrum sparsiflorum      Eriastrum sparsiflorum KANU00229173
672       Eriastrum sparsiflorum        Eriastrum sparsiflorum FLAS190755
673       Eriastrum sparsiflorum      Eriastrum sparsiflorum KANU00229173
674           Eriastrum virgatum          Eriastrum virgatum KANU00229180
675           Eriastrum virgatum          Eriastrum virgatum KANU00229180
676           Eriastrum virgatum          Eriastrum virgatum KANU00229176
677           Eriastrum virgatum                                     <NA>
678           Eriastrum virgatum            Eriastrum virgatum KSC0126378
679           Eriastrum virgatum          Eriastrum virgatum KANU00229176
680           Eriastrum virgatum          Eriastrum virgatum KANU00229180
681           Eriastrum virgatum          Eriastrum virgatum KANU00229176
682           Eriastrum wilcoxii          Eriastrum wilcoxii KANU00229181
683           Eriastrum wilcoxii            Eriastrum wilcoxii KSC0126373
684           Eriastrum wilcoxii                                     <NA>
685           Eriastrum wilcoxii             Eriastrum wilcoxii FLAS20307
686           Eriastrum wilcoxii             Eriastrum wilcoxii FLAS20307
687           Eriastrum wilcoxii             Eriastrum wilcoxii FLAS20307
688           Eriastrum wilcoxii            Eriastrum wilcoxii KSC0126373
689           Eriastrum wilcoxii            Eriastrum wilcoxii KSC0126373
690         Fouquieria splendens           Fouquieria splendens MO2294377
691         Fouquieria splendens                                     <NA>
692         Fouquieria splendens           Fouquieria splendens MO1963133
693         Fouquieria splendens           Fouquieria splendens MO1963130
694         Fouquieria splendens           Fouquieria splendens MO1963130
695         Fouquieria splendens           Fouquieria splendens MO1963133
696         Fouquieria splendens           Fouquieria splendens MO1963133
697         Fouquieria splendens           Fouquieria splendens MO2294377
698         Fouquieria splendens           Fouquieria splendens MO2294377
699         Fouquieria splendens           Fouquieria splendens MO1963130
700              Gilia aliquanta               Gilia aliquanta MO05091925
701              Gilia aliquanta                Gilia aliquanta MO2985153
702              Gilia aliquanta               Gilia aliquanta MO05091925
703              Gilia aliquanta               Gilia aliquanta MO05091926
704              Gilia aliquanta                                     <NA>
705              Gilia aliquanta               Gilia aliquanta MO05091925
706              Gilia aliquanta                Gilia aliquanta MO2985153
707              Gilia aliquanta                Gilia aliquanta MO2985153
708              Gilia aliquanta               Gilia aliquanta MO05091926
709              Gilia aliquanta               Gilia aliquanta MO05091926
710             Gilia angelensis              Gilia angelensis KANU352958
711             Gilia angelensis              Gilia angelensis FLAS124815
712             Gilia angelensis              Gilia angelensis KANU352958
713             Gilia angelensis              Gilia angelensis FLAS103491
714             Gilia angelensis              Gilia angelensis FLAS103491
715             Gilia angelensis                                     <NA>
716             Gilia angelensis            Gilia angelensis KANU00237600
717             Gilia angelensis              Gilia angelensis FLAS103491
718             Gilia angelensis            Gilia angelensis KANU00237600
719             Gilia angelensis            Gilia angelensis KANU00237600
720             Gilia angelensis              Gilia angelensis FLAS124815
721             Gilia angelensis              Gilia angelensis FLAS124815
722             Gilia angelensis              Gilia angelensis KANU352958
723     Gilia austrooccidentalis                                     <NA>
724     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761527
725     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761527
726     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761740
727     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761740
728     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761527
729     Gilia austrooccidentalis     Gilia austrooccidentalis RSABG761740
730                   Gilia cana                    Gilia cana KSC0126225
731                   Gilia cana                    Gilia cana KSC0126225
732                   Gilia cana                                     <NA>
733                   Gilia cana                  Gilia cana KANU00237606
734                   Gilia cana                    Gilia cana KSC0126225
735                   Gilia cana                  Gilia cana KANU00237606
736                   Gilia cana                  Gilia cana KANU00237606
737               Gilia capitata                Gilia capitata KSC0126218
738               Gilia capitata        Gilia capitata KANU365398 flowers
739               Gilia capitata                Gilia capitata KSC0126218
740               Gilia capitata                Gilia capitata KSC0126215
741               Gilia capitata                Gilia capitata KSC0126215
742               Gilia capitata        Gilia capitata KANU365398 flowers
743               Gilia capitata                 Gilia capitata FLAS20303
744               Gilia capitata                 Gilia capitata FLAS55667
745               Gilia capitata                Gilia capitata KANU365398
746               Gilia capitata                 Gilia capitata FLAS55667
747               Gilia capitata                Gilia capitata KANU365398
748               Gilia capitata                 Gilia capitata FLAS55668
749               Gilia capitata                Gilia capitata KSC0126218
750               Gilia capitata                                     <NA>
751               Gilia capitata                Gilia capitata KANU365398
752               Gilia capitata                Gilia capitata KSC0126215
753               Gilia capitata              Gilia capitata KANU00237624
754               Gilia capitata                 Gilia capitata FLAS20303
755               Gilia capitata              Gilia capitata KANU00237624
756               Gilia capitata                 Gilia capitata FLAS55667
757               Gilia capitata                 Gilia capitata FLAS20303
758               Gilia capitata        Gilia capitata KANU365398 flowers
759               Gilia capitata              Gilia capitata KANU00237624
760               Gilia capitata                 Gilia capitata FLAS55668
761               Gilia capitata                 Gilia capitata FLAS55668
762               Gilia clivorum                Gilia clivorum KANU365951
763               Gilia clivorum              Gilia clivorum KANU00237635
764               Gilia clivorum                Gilia clivorum KANU365951
765               Gilia clivorum                                     <NA>
766               Gilia clivorum              Gilia clivorum KANU00237635
767               Gilia clivorum              Gilia clivorum KANU00237635
768               Gilia clivorum                Gilia clivorum KANU365951
769                Gilia clokeyi               Gilia clokeyi KANU00237636
770                Gilia clokeyi               Gilia clokeyi KANU00237636
771                Gilia clokeyi               Gilia clokeyi KANU00237637
772                Gilia clokeyi               Gilia clokeyi KANU00237636
773                Gilia clokeyi                                     <NA>
774                Gilia clokeyi               Gilia clokeyi KANU00237637
775                Gilia clokeyi               Gilia clokeyi KANU00237637
776            Gilia crassifolia                                     <NA>
777            Gilia crassifolia               Gilia crassifolia MO910596
778            Gilia crassifolia               Gilia crassifolia MO910596
779            Gilia crassifolia              Gilia crassifolia MO6217685
780            Gilia crassifolia               Gilia crassifolia MO910596
781            Gilia crassifolia              Gilia crassifolia MO6217685
782            Gilia crassifolia              Gilia crassifolia MO6217685
783            Gilia crassifolia              Gilia crassifolia MO2415702
784            Gilia crassifolia              Gilia crassifolia MO2415702
785            Gilia crassifolia              Gilia crassifolia MO2415702
786              Gilia diegensis                 Gilia diegensis UF124844
787              Gilia diegensis               Gilia diegensis FLAS124844
788              Gilia diegensis               Gilia diegensis FLAS124844
789              Gilia diegensis               Gilia diegensis FLAS124844
790              Gilia diegensis                 Gilia diegensis UF124844
791              Gilia diegensis                 Gilia diegensis UF124844
792              Gilia diegensis                                     <NA>
793            Gilia inconspicua             Gilia inconspicua KSC0126288
794            Gilia inconspicua                Gilia inconspicua UF71466
795            Gilia inconspicua             Gilia inconspicua KSC0126288
796            Gilia inconspicua             Gilia inconspicua KSC0126288
797            Gilia inconspicua           Gilia inconspicua KANU00237648
798            Gilia inconspicua              Gilia inconspicua FLAS71466
799            Gilia inconspicua                Gilia inconspicua UF71466
800            Gilia inconspicua                                     <NA>
801            Gilia inconspicua              Gilia inconspicua FLAS71466
802            Gilia inconspicua             Gilia inconspicua KSC0126285
803            Gilia inconspicua                Gilia inconspicua UF71466
804            Gilia inconspicua           Gilia inconspicua KANU00237648
805            Gilia inconspicua             Gilia inconspicua KSC0126285
806            Gilia inconspicua           Gilia inconspicua KANU00237648
807            Gilia inconspicua              Gilia inconspicua FLAS71466
808            Gilia inconspicua             Gilia inconspicua KANU354829
809            Gilia inconspicua             Gilia inconspicua KANU354833
810            Gilia inconspicua             Gilia inconspicua KANU354829
811            Gilia inconspicua             Gilia inconspicua KANU354829
812            Gilia inconspicua             Gilia inconspicua KSC0126285
813            Gilia inconspicua             Gilia inconspicua KANU354833
814            Gilia inconspicua             Gilia inconspicua KSC0126280
815            Gilia inconspicua             Gilia inconspicua KSC0126280
816            Gilia inconspicua             Gilia inconspicua KANU354833
817            Gilia inconspicua             Gilia inconspicua KSC0126280
818               Gilia interior                                     <NA>
819               Gilia interior                 Gilia interior MO2684705
820               Gilia interior                 Gilia interior MO2684705
821               Gilia interior                 Gilia interior MO2684705
822              Gilia laciniata              Gilia laciniata RSABG670177
823              Gilia laciniata              Gilia laciniata RSABG742105
824              Gilia laciniata              Gilia laciniata RSABG670177
825              Gilia laciniata              Gilia laciniata RSABG742105
826              Gilia laciniata                                     <NA>
827              Gilia laciniata              Gilia laciniata RSABG670177
828              Gilia laciniata              Gilia laciniata RSABG742105
829              Gilia latiflora             Gilia latiflora KANU00237664
830              Gilia latiflora             Gilia latiflora KANU00237664
831              Gilia latiflora             Gilia latiflora KANU00237670
832              Gilia latiflora             Gilia latiflora KANU00237664
833              Gilia latiflora             Gilia latiflora KANU00237670
834              Gilia latiflora             Gilia latiflora KANU00237665
835              Gilia latiflora                                     <NA>
836              Gilia latiflora             Gilia latiflora KANU00237670
837              Gilia latiflora             Gilia latiflora KANU00237665
838              Gilia latiflora             Gilia latiflora KANU00237665
839              Gilia leptantha                Gilia leptantha MO1971214
840              Gilia leptantha                                     <NA>
841              Gilia leptantha                Gilia leptantha MO1971214
842              Gilia leptantha                Gilia leptantha MO1971214
843                 Gilia malior                Gilia malior KANU00329179
844                 Gilia malior                                     <NA>
845                 Gilia malior                Gilia malior KANU00329179
846                 Gilia malior                Gilia malior KANU00329179
847               Gilia mexicana                 Gilia mexicana MO2926797
848               Gilia mexicana                 Gilia mexicana MO2926797
849               Gilia mexicana                                     <NA>
850               Gilia mexicana                 Gilia mexicana MO2926797
851           Gilia millefoliata          Gilia millefoliata KANU00237704
852           Gilia millefoliata          Gilia millefoliata KANU00237704
853           Gilia millefoliata          Gilia millefoliata KANU00237704
854           Gilia millefoliata                                     <NA>
855                  Gilia minor                 Gilia minor KANU00237705
856                  Gilia minor                 Gilia minor KANU00237705
857                  Gilia minor                                     <NA>
858                  Gilia minor                 Gilia minor KANU00237705
859                Gilia nevinii                  Gilia nevinii MO2754968
860                Gilia nevinii                  Gilia nevinii MO2397657
861                Gilia nevinii                                     <NA>
862                Gilia nevinii                  Gilia nevinii MO2754968
863                Gilia nevinii                  Gilia nevinii MO2397657
864                Gilia nevinii                  Gilia nevinii MO2397657
865                Gilia nevinii                  Gilia nevinii MO2753771
866                Gilia nevinii                  Gilia nevinii MO2754968
867              Gilia salticola             Gilia salticola KANU00318641
868              Gilia salticola               Gilia salticola KANU363635
869              Gilia salticola                                     <NA>
870              Gilia salticola               Gilia salticola KANU363635
871              Gilia salticola               Gilia salticola KANU363635
872              Gilia salticola             Gilia salticola KANU00318641
873              Gilia salticola             Gilia salticola KANU00318641
874             Gilia scopulorum            Gilia scopulorum KANU00237764
875             Gilia scopulorum            Gilia scopulorum KANU00237764
876             Gilia scopulorum                                     <NA>
877             Gilia scopulorum            Gilia scopulorum KANU00237764
878             Gilia scopulorum           Gilia scopulorum KANU002377765
879             Gilia scopulorum           Gilia scopulorum KANU002377765
880             Gilia scopulorum           Gilia scopulorum KANU002377765
881                Gilia sinuata                 Gilia sinuata KSC0126407
882                Gilia sinuata               Gilia sinuata KANU00237779
883                Gilia sinuata                 Gilia sinuata KANU365726
884                Gilia sinuata               Gilia sinuata KANU00237781
885                Gilia sinuata                                     <NA>
886                Gilia sinuata                    Gilia sinuata UF79291
887                Gilia sinuata               Gilia sinuata KANU00237779
888                Gilia sinuata               Gilia sinuata KANU00237781
889                Gilia sinuata                    Gilia sinuata UF79291
890                Gilia sinuata               Gilia sinuata KANU00237779
891                Gilia sinuata                 Gilia sinuata KSC0126407
892                Gilia sinuata               Gilia sinuata KANU00237781
893                Gilia sinuata                 Gilia sinuata KANU365726
894                Gilia sinuata                 Gilia sinuata KSC0126407
895                Gilia sinuata                 Gilia sinuata KANU365726
896                Gilia sinuata                    Gilia sinuata UF79291
897               Gilia stellata              Gilia stellata KANU00237789
898               Gilia stellata              Gilia stellata KANU00237788
899               Gilia stellata              Gilia stellata KANU00237788
900               Gilia stellata                  Gilia stellata UF106799
901               Gilia stellata              Gilia stellata KANU00237787
902               Gilia stellata              Gilia stellata KANU00237788
903               Gilia stellata              Gilia stellata KANU00237789
904               Gilia stellata              Gilia stellata KANU00237787
905               Gilia stellata                  Gilia stellata UF106799
906               Gilia stellata              Gilia stellata KANU00237787
907               Gilia stellata                  Gilia stellata UF106799
908               Gilia stellata                                     <NA>
909               Gilia stellata              Gilia stellata KANU00237789
910             Gilia tenuiflora    Gilia tenuiflora KANU00237663 flowers
911             Gilia tenuiflora    Gilia tenuiflora KANU00237663 flowers
912             Gilia tenuiflora            Gilia tenuiflora KANU00237808
913             Gilia tenuiflora            Gilia tenuiflora KANU00237808
914             Gilia tenuiflora            Gilia tenuiflora KANU00237663
915             Gilia tenuiflora    Gilia tenuiflora KANU00237663 flowers
916             Gilia tenuiflora              Gilia tenuiflora KSC0126388
917             Gilia tenuiflora            Gilia tenuiflora KANU00237809
918             Gilia tenuiflora                                     <NA>
919             Gilia tenuiflora            Gilia tenuiflora KANU00237809
920             Gilia tenuiflora              Gilia tenuiflora KSC0126388
921             Gilia tenuiflora            Gilia tenuiflora KANU00237810
922             Gilia tenuiflora            Gilia tenuiflora KANU00237808
923             Gilia tenuiflora              Gilia tenuiflora KSC0126388
924             Gilia tenuiflora            Gilia tenuiflora KANU00237810
925             Gilia tenuiflora            Gilia tenuiflora KANU00237809
926             Gilia tenuiflora            Gilia tenuiflora KANU00237663
927             Gilia tenuiflora            Gilia tenuiflora KANU00237663
928             Gilia tenuiflora            Gilia tenuiflora KANU00237810
929           Gilia transmontana          Gilia transmontana KANU00237813
930           Gilia transmontana          Gilia transmontana KANU00237813
931           Gilia transmontana                                     <NA>
932           Gilia transmontana          Gilia transmontana KANU00237813
933               Gilia tricolor                Gilia tricolor KANU365399
934               Gilia tricolor              Gilia tricolor KANU00237815
935               Gilia tricolor              Gilia tricolor KANU00329192
936               Gilia tricolor              Gilia tricolor KANU00329192
937               Gilia tricolor              Gilia tricolor KANU00329192
938               Gilia tricolor                Gilia tricolor KSC0126382
939               Gilia tricolor                  Gilia tricolor UF122313
940               Gilia tricolor              Gilia tricolor KANU00237815
941               Gilia tricolor              Gilia tricolor KANU00237815
942               Gilia tricolor                  Gilia tricolor UF122313
943               Gilia tricolor                Gilia tricolor KSC0126382
944               Gilia tricolor                Gilia tricolor KANU365399
945               Gilia tricolor                Gilia tricolor KANU365399
946               Gilia tricolor                                     <NA>
947               Gilia tricolor                  Gilia tricolor UF122313
948               Gilia tricolor                Gilia tricolor KSC0126382
949                Gilia tweedyi               Gilia tweedyi KANU00237816
950                Gilia tweedyi               Gilia tweedyi KANU00307908
951                Gilia tweedyi               Gilia tweedyi KANU00307908
952                Gilia tweedyi                                     <NA>
953                Gilia tweedyi               Gilia tweedyi KANU00237816
954                Gilia tweedyi                 Gilia tweedyi KSC0126379
955                Gilia tweedyi               Gilia tweedyi KANU00237816
956                Gilia tweedyi                 Gilia tweedyi KSC0126379
957                Gilia tweedyi                 Gilia tweedyi KSC0126379
958                Gilia tweedyi               Gilia tweedyi KANU00307908
959           Gilia valdiviensis           Gilia valdiviensis RSABG689440
960           Gilia valdiviensis           Gilia valdiviensis RSABG689440
961           Gilia valdiviensis           Gilia valdiviensis RSABG742098
962           Gilia valdiviensis           Gilia valdiviensis RSABG742098
963           Gilia valdiviensis           Gilia valdiviensis RSABG689440
964           Gilia valdiviensis                                     <NA>
965          Giliastrum foetidum            Giliastrum foetidum MO2387040
966          Giliastrum foetidum            Giliastrum foetidum MO3381110
967          Giliastrum foetidum            Giliastrum foetidum MO5691534
968          Giliastrum foetidum            Giliastrum foetidum MO5691534
969          Giliastrum foetidum            Giliastrum foetidum MO2387040
970          Giliastrum foetidum            Giliastrum foetidum MO3381110
971          Giliastrum foetidum            Giliastrum foetidum MO2387040
972          Giliastrum foetidum                                     <NA>
973          Giliastrum foetidum            Giliastrum foetidum MO5691534
974           Giliastrum incisum          Giliastrum incisum KANU00237644
975           Giliastrum incisum          Giliastrum incisum KANU00237644
976           Giliastrum incisum               Giliastrum incisum UF97704
977           Giliastrum incisum          Giliastrum incisum KANU00237644
978           Giliastrum incisum            Giliastrum incisum KANU349179
979           Giliastrum incisum              Giliastrum incisum UF151811
980           Giliastrum incisum               Giliastrum incisum UF97704
981           Giliastrum incisum            Giliastrum incisum KANU349179
982           Giliastrum incisum            Giliastrum incisum KANU349179
983           Giliastrum incisum              Giliastrum incisum UF151811
984           Giliastrum incisum                                     <NA>
985           Giliastrum incisum               Giliastrum incisum UF97704
986           Giliastrum incisum            Giliastrum incisum FLAS151811
987           Giliastrum incisum              Giliastrum incisum UF151811
988           Giliastrum incisum            Giliastrum incisum FLAS151811
989           Giliastrum incisum            Giliastrum incisum FLAS151811
990            Giliastrum ludens              Giliastrum ludens MO3693030
991            Giliastrum ludens              Giliastrum ludens MO2375107
992            Giliastrum ludens              Giliastrum ludens MO2375107
993            Giliastrum ludens              Giliastrum ludens MO2375107
994            Giliastrum ludens                                     <NA>
995            Giliastrum ludens              Giliastrum ludens MO3693030
996            Giliastrum ludens              Giliastrum ludens MO3693030
997         Giliastrum rigidulum        Giliastrum rigidulum KANU00237763
998         Giliastrum rigidulum                                     <NA>
999         Giliastrum rigidulum        Giliastrum rigidulum KANU00237763
1000        Giliastrum rigidulum        Giliastrum rigidulum KANU00237744
1001        Giliastrum rigidulum        Giliastrum rigidulum KANU00237744
1002        Giliastrum rigidulum        Giliastrum rigidulum KANU00237744
1003        Giliastrum rigidulum        Giliastrum rigidulum KANU00237763
1004      Gymnosteris nudicaulis      Gymnosteris nudicaulis KANU00238204
1005      Gymnosteris nudicaulis      Gymnosteris nudicaulis KANU00238204
1006      Gymnosteris nudicaulis      Gymnosteris nudicaulis KANU00238204
1007      Gymnosteris nudicaulis                                     <NA>
1008         Gymnosteris parvula         Gymnosteris parvula KANU00238206
1009         Gymnosteris parvula                                     <NA>
1010         Gymnosteris parvula         Gymnosteris parvula KANU00238205
1011         Gymnosteris parvula         Gymnosteris parvula KANU00238206
1012         Gymnosteris parvula         Gymnosteris parvula KANU00238206
1013         Gymnosteris parvula         Gymnosteris parvula KANU00238205
1014         Gymnosteris parvula         Gymnosteris parvula KANU00238205
1015         Ipomopsis aggregata           Ipomopsis aggregata KSC0126486
1016         Ipomopsis aggregata            Ipomopsis aggregata FLAS72005
1017         Ipomopsis aggregata            Ipomopsis aggregata FLAS72005
1018         Ipomopsis aggregata            Ipomopsis aggregata FLAS79378
1019         Ipomopsis aggregata            Ipomopsis aggregata FLAS79378
1020         Ipomopsis aggregata                                     <NA>
1021         Ipomopsis aggregata           Ipomopsis aggregata KSC0126486
1022         Ipomopsis aggregata            Ipomopsis aggregata FLAS72005
1023         Ipomopsis aggregata           Ipomopsis aggregata KSC0126486
1024         Ipomopsis aggregata            Ipomopsis aggregata FLAS79378
1025         Ipomopsis arizonica         Ipomopsis arizonica KANU00242038
1026         Ipomopsis arizonica         Ipomopsis arizonica KANU00242026
1027         Ipomopsis arizonica         Ipomopsis arizonica KANU00242034
1028         Ipomopsis arizonica         Ipomopsis arizonica KANU00242034
1029         Ipomopsis arizonica         Ipomopsis arizonica KANU00242026
1030         Ipomopsis arizonica           Ipomopsis arizonica KSC0126427
1031         Ipomopsis arizonica            Ipomopsis arizonica RMH369047
1032         Ipomopsis arizonica           Ipomopsis arizonica KSC0126427
1033         Ipomopsis arizonica         Ipomopsis arizonica KANU00242034
1034         Ipomopsis arizonica           Ipomopsis arizonica KSC0126425
1035         Ipomopsis arizonica                                     <NA>
1036         Ipomopsis arizonica            Ipomopsis arizonica RMH152770
1037         Ipomopsis arizonica            Ipomopsis arizonica RMH369047
1038         Ipomopsis arizonica         Ipomopsis arizonica KANU00242038
1039         Ipomopsis arizonica           Ipomopsis arizonica KSC0126425
1040         Ipomopsis arizonica            Ipomopsis arizonica RMH369047
1041         Ipomopsis arizonica            Ipomopsis arizonica RMH152770
1042         Ipomopsis arizonica           Ipomopsis arizonica KSC0126427
1043         Ipomopsis arizonica           Ipomopsis arizonica KSC0126425
1044         Ipomopsis arizonica            Ipomopsis arizonica RMH152770
1045         Ipomopsis arizonica         Ipomopsis arizonica KANU00242038
1046         Ipomopsis arizonica         Ipomopsis arizonica KANU00242026
1047          Ipomopsis congesta            Ipomopsis congesta FLAS202042
1048          Ipomopsis congesta             Ipomopsis congesta FLAS43707
1049          Ipomopsis congesta            Ipomopsis congesta FLAS202042
1050          Ipomopsis congesta               Ipomopsis congesta UF43707
1051          Ipomopsis congesta            Ipomopsis congesta FLAS202042
1052          Ipomopsis congesta             Ipomopsis congesta FLAS43707
1053          Ipomopsis congesta             Ipomopsis congesta FLAS43707
1054          Ipomopsis congesta               Ipomopsis congesta UF43707
1055          Ipomopsis congesta               Ipomopsis congesta UF43707
1056          Ipomopsis congesta                                     <NA>
1057            Ipomopsis effusa                                     <NA>
1058            Ipomopsis effusa                Ipomopsis effusa ASU36119
1059            Ipomopsis effusa                Ipomopsis effusa ASU36119
1060            Ipomopsis effusa            Ipomopsis effusa KANU00242123
1061            Ipomopsis effusa                Ipomopsis effusa ASU36119
1062            Ipomopsis effusa            Ipomopsis effusa KANU00242123
1063            Ipomopsis effusa            Ipomopsis effusa KANU00242123
1064       Ipomopsis gossypifera          Ipomopsis gossypifera MO2990603
1065       Ipomopsis gossypifera         Ipomopsis gossypifera MO04674924
1066       Ipomopsis gossypifera          Ipomopsis gossypifera MO2990603
1067       Ipomopsis gossypifera          Ipomopsis gossypifera MO2990603
1068       Ipomopsis gossypifera         Ipomopsis gossypifera MO04674924
1069       Ipomopsis gossypifera         Ipomopsis gossypifera MO04674924
1070       Ipomopsis gossypifera          Ipomopsis gossypifera MO5695559
1071       Ipomopsis gossypifera                                     <NA>
1072       Ipomopsis gossypifera          Ipomopsis gossypifera MO5695559
1073       Ipomopsis gossypifera          Ipomopsis gossypifera MO5695559
1074        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242127
1075        Ipomopsis gunnisonii           Ipomopsis gunnisonii UNM102099
1076        Ipomopsis gunnisonii           Ipomopsis gunnisonii UNM102099
1077        Ipomopsis gunnisonii           Ipomopsis gunnisonii UNM102099
1078        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242126
1079        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242126
1080        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242125
1081        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242127
1082        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242127
1083        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242126
1084        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242125
1085        Ipomopsis gunnisonii        Ipomopsis gunnisonii KANU00242125
1086        Ipomopsis gunnisonii                                     <NA>
1087           Ipomopsis guttata           Ipomopsis guttata KANU00242128
1088           Ipomopsis guttata              Ipomopsis guttata ASU136142
1089           Ipomopsis guttata                                     <NA>
1090           Ipomopsis guttata           Ipomopsis guttata KANU00242128
1091           Ipomopsis guttata              Ipomopsis guttata ASU203122
1092           Ipomopsis guttata              Ipomopsis guttata ASU136142
1093           Ipomopsis guttata              Ipomopsis guttata ASU203122
1094           Ipomopsis guttata           Ipomopsis guttata KANU00242128
1095           Ipomopsis guttata              Ipomopsis guttata ASU203122
1096           Ipomopsis guttata              Ipomopsis guttata ASU136142
1097         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS57281
1098         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS20309
1099         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS54612
1100         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS57281
1101         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS20309
1102         Ipomopsis laxiflora           Ipomopsis laxiflora FLAS134450
1103         Ipomopsis laxiflora           Ipomopsis laxiflora FLAS134450
1104         Ipomopsis laxiflora           Ipomopsis laxiflora FLAS134450
1105         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS20309
1106         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS57281
1107         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS54612
1108         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242134
1109         Ipomopsis laxiflora            Ipomopsis laxiflora FLAS54612
1110         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242134
1111         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242134
1112         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242137
1113         Ipomopsis laxiflora                                     <NA>
1114         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242137
1115         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242136
1116         Ipomopsis laxiflora         Ipomopsis laxiflora KANU00242136
1117        Ipomopsis longiflora          Ipomopsis longiflora KANU365648
1118        Ipomopsis longiflora          Ipomopsis longiflora KANU365648
1119        Ipomopsis longiflora          Ipomopsis longiflora KANU365648
1120        Ipomopsis longiflora          Ipomopsis longiflora KANU365567
1121        Ipomopsis longiflora          Ipomopsis longiflora KANU365567
1122        Ipomopsis longiflora          Ipomopsis longiflora KANU365567
1123        Ipomopsis longiflora            Ipomopsis longiflora UNM96003
1124        Ipomopsis longiflora         Ipomopsis longiflora KSC01226429
1125        Ipomopsis longiflora                                     <NA>
1126        Ipomopsis longiflora        Ipomopsis longiflora KANU00314448
1127        Ipomopsis longiflora          Ipomopsis longiflora KSC0126416
1128        Ipomopsis longiflora          Ipomopsis longiflora KSC0126416
1129        Ipomopsis longiflora        Ipomopsis longiflora KANU00314448
1130        Ipomopsis longiflora         Ipomopsis longiflora KSC01226429
1131        Ipomopsis longiflora            Ipomopsis longiflora UNM96003
1132        Ipomopsis longiflora            Ipomopsis longiflora UNM96003
1133        Ipomopsis longiflora          Ipomopsis longiflora KANU367093
1134        Ipomopsis longiflora        Ipomopsis longiflora KANU00314448
1135        Ipomopsis longiflora          Ipomopsis longiflora KANU367093
1136        Ipomopsis longiflora          Ipomopsis longiflora KSC0126416
1137        Ipomopsis longiflora          Ipomopsis longiflora KANU367093
1138        Ipomopsis longiflora         Ipomopsis longiflora KSC01226429
1139          Ipomopsis macombii             Ipomopsis macombii MO3537828
1140          Ipomopsis macombii               Ipomopsis macombii UF54612
1141          Ipomopsis macombii               Ipomopsis macombii UF54612
1142          Ipomopsis macombii             Ipomopsis macombii MO3537828
1143          Ipomopsis macombii             Ipomopsis macombii MO3537828
1144          Ipomopsis macombii               Ipomopsis macombii UF20309
1145          Ipomopsis macombii            Ipomopsis macombii MO04942860
1146          Ipomopsis macombii               Ipomopsis macombii UF20309
1147          Ipomopsis macombii               Ipomopsis macombii UF54612
1148          Ipomopsis macombii             Ipomopsis macombii MO3667297
1149          Ipomopsis macombii               Ipomopsis macombii UF57281
1150          Ipomopsis macombii             Ipomopsis macombii MO3667297
1151          Ipomopsis macombii               Ipomopsis macombii UF20309
1152          Ipomopsis macombii             Ipomopsis macombii MO3667297
1153          Ipomopsis macombii            Ipomopsis macombii MO04942860
1154          Ipomopsis macombii            Ipomopsis macombii MO04942860
1155          Ipomopsis macombii               Ipomopsis macombii UF57281
1156          Ipomopsis macombii                                     <NA>
1157          Ipomopsis macombii               Ipomopsis macombii UF57281
1158       Ipomopsis macrosiphon        Ipomopsis macrosiphon DES00054030
1159       Ipomopsis macrosiphon        Ipomopsis macrosiphon DES00054030
1160       Ipomopsis macrosiphon        Ipomopsis macrosiphon DES00054030
1161       Ipomopsis macrosiphon                                     <NA>
1162       Ipomopsis macrosiphon        Ipomopsis macrosiphon DES00066249
1163       Ipomopsis macrosiphon        Ipomopsis macrosiphon DES00066249
1164       Ipomopsis macrosiphon        Ipomopsis macrosiphon DES00066249
1165           Ipomopsis pinnata            Ipomopsis pinnata RSABG743768
1166           Ipomopsis pinnata            Ipomopsis pinnata RSABG586885
1167           Ipomopsis pinnata            Ipomopsis pinnata RSABG743768
1168           Ipomopsis pinnata               Ipomopsis pinnata UNM81872
1169           Ipomopsis pinnata            Ipomopsis pinnata RSABG743768
1170           Ipomopsis pinnata               Ipomopsis pinnata ASU79436
1171           Ipomopsis pinnata              Ipomopsis pinnata UNM123448
1172           Ipomopsis pinnata               Ipomopsis pinnata ASU79436
1173           Ipomopsis pinnata            Ipomopsis pinnata RSABG586885
1174           Ipomopsis pinnata                                     <NA>
1175           Ipomopsis pinnata               Ipomopsis pinnata UNM81872
1176           Ipomopsis pinnata               Ipomopsis pinnata ASU79436
1177           Ipomopsis pinnata              Ipomopsis pinnata UNM123448
1178           Ipomopsis pinnata            Ipomopsis pinnata RSABG586885
1179           Ipomopsis pinnata               Ipomopsis pinnata UNM81872
1180           Ipomopsis pinnata              Ipomopsis pinnata UNM123448
1181         Ipomopsis polyantha         Ipomopsis polyantha KANU00242201
1182         Ipomopsis polyantha            Ipomopsis polyantha MO5188647
1183         Ipomopsis polyantha              Ipomopsis polyantha 1199313
1184         Ipomopsis polyantha              Ipomopsis polyantha 1199313
1185         Ipomopsis polyantha           Ipomopsis polyantha FLAS134452
1186         Ipomopsis polyantha            Ipomopsis polyantha RMH393646
1187         Ipomopsis polyantha            Ipomopsis polyantha MO5188647
1188         Ipomopsis polyantha              Ipomopsis polyantha 1199313
1189         Ipomopsis polyantha            Ipomopsis polyantha MO5188647
1190         Ipomopsis polyantha         Ipomopsis polyantha KANU00242201
1191         Ipomopsis polyantha         Ipomopsis polyantha KANU00242201
1192         Ipomopsis polyantha           Ipomopsis polyantha FLAS134452
1193         Ipomopsis polyantha            Ipomopsis polyantha RMH393647
1194         Ipomopsis polyantha             Ipomopsis polyantha MO890264
1195         Ipomopsis polyantha           Ipomopsis polyantha FLAS134452
1196         Ipomopsis polyantha            Ipomopsis polyantha RMH393647
1197         Ipomopsis polyantha             Ipomopsis polyantha MO890264
1198         Ipomopsis polyantha              Ipomopsis polyantha UF40538
1199         Ipomopsis polyantha            Ipomopsis polyantha RMH393646
1200         Ipomopsis polyantha              Ipomopsis polyantha UF40538
1201         Ipomopsis polyantha            Ipomopsis polyantha RMH393647
1202         Ipomopsis polyantha            Ipomopsis polyantha RMH393646
1203         Ipomopsis polyantha              Ipomopsis polyantha UF40538
1204         Ipomopsis polyantha             Ipomopsis polyantha MO890264
1205         Ipomopsis polyantha                                     <NA>
1206        Ipomopsis polycladon        Ipomopsis polycladon KANU00242202
1207        Ipomopsis polycladon        Ipomopsis polycladon KANU00242209
1208        Ipomopsis polycladon        Ipomopsis polycladon KANU00242209
1209        Ipomopsis polycladon        Ipomopsis polycladon KANU00242207
1210        Ipomopsis polycladon        Ipomopsis polycladon KANU00242202
1211        Ipomopsis polycladon            Ipomopsis polycladon UF202043
1212        Ipomopsis polycladon        Ipomopsis polycladon KANU00242209
1213        Ipomopsis polycladon            Ipomopsis polycladon UF202043
1214        Ipomopsis polycladon        Ipomopsis polycladon KANU00242207
1215        Ipomopsis polycladon        Ipomopsis polycladon KANU00242202
1216        Ipomopsis polycladon            Ipomopsis polycladon UF202043
1217        Ipomopsis polycladon                                     <NA>
1218          Ipomopsis pringlei        Ipomopsis pringlei Arizona 342915
1219          Ipomopsis pringlei           Ipomopsis pringlei RSABG735405
1220          Ipomopsis pringlei           Ipomopsis pringlei RSABG743767
1221          Ipomopsis pringlei           Ipomopsis pringlei RSABG735405
1222          Ipomopsis pringlei        Ipomopsis pringlei Arizona 342915
1223          Ipomopsis pringlei           Ipomopsis pringlei RSABG743767
1224          Ipomopsis pringlei        Ipomopsis pringlei Arizona 342915
1225          Ipomopsis pringlei           Ipomopsis pringlei RSABG743767
1226          Ipomopsis pringlei           Ipomopsis pringlei RSABG735405
1227          Ipomopsis pringlei                                     <NA>
1228            Ipomopsis pumila                Ipomopsis pumila UNM88300
1229            Ipomopsis pumila              Ipomopsis pumila KSC0127470
1230            Ipomopsis pumila                                     <NA>
1231            Ipomopsis pumila                Ipomopsis pumila UNM93840
1232            Ipomopsis pumila                Ipomopsis pumila UNM93840
1233            Ipomopsis pumila            Ipomopsis pumila KANU00242210
1234            Ipomopsis pumila            Ipomopsis pumila KANU00242213
1235            Ipomopsis pumila               Ipomopsis pumila UNM113120
1236            Ipomopsis pumila               Ipomopsis pumila UNM114147
1237            Ipomopsis pumila            Ipomopsis pumila KANU00242210
1238            Ipomopsis pumila               Ipomopsis pumila UNM113120
1239            Ipomopsis pumila                Ipomopsis pumila UNM88300
1240            Ipomopsis pumila            Ipomopsis pumila KANU00242210
1241            Ipomopsis pumila                Ipomopsis pumila UNM93840
1242            Ipomopsis pumila            Ipomopsis pumila KANU00242213
1243            Ipomopsis pumila               Ipomopsis pumila UNM114147
1244            Ipomopsis pumila            Ipomopsis pumila KANU00242213
1245            Ipomopsis pumila               Ipomopsis pumila UNM113120
1246            Ipomopsis pumila               Ipomopsis pumila UNM114147
1247            Ipomopsis pumila              Ipomopsis pumila KSC0127470
1248            Ipomopsis pumila                Ipomopsis pumila UNM88300
1249            Ipomopsis pumila              Ipomopsis pumila KSC0127470
1250           Ipomopsis roseata              Ipomopsis roseata RMH344766
1251           Ipomopsis roseata              Ipomopsis roseata RMH744381
1252           Ipomopsis roseata              Ipomopsis roseata RMH344766
1253           Ipomopsis roseata           Ipomopsis roseata KANU00242219
1254           Ipomopsis roseata           Ipomopsis roseata KANU00242218
1255           Ipomopsis roseata           Ipomopsis roseata KANU00242219
1256           Ipomopsis roseata              Ipomopsis roseata RMH744381
1257           Ipomopsis roseata              Ipomopsis roseata RMH344766
1258           Ipomopsis roseata              Ipomopsis roseata RMH785469
1259           Ipomopsis roseata                                     <NA>
1260           Ipomopsis roseata           Ipomopsis roseata KANU00242218
1261           Ipomopsis roseata              Ipomopsis roseata RMH368362
1262           Ipomopsis roseata              Ipomopsis roseata RMH785469
1263           Ipomopsis roseata              Ipomopsis roseata RMH744381
1264           Ipomopsis roseata              Ipomopsis roseata RMH785469
1265           Ipomopsis roseata              Ipomopsis roseata RMH368362
1266           Ipomopsis roseata              Ipomopsis roseata RMH368362
1267           Ipomopsis roseata           Ipomopsis roseata KANU00242219
1268           Ipomopsis roseata           Ipomopsis roseata KANU00242218
1269             Ipomopsis rubra               Ipomopsis rubra KANU352624
1270             Ipomopsis rubra               Ipomopsis rubra KANU352624
1271             Ipomopsis rubra             Ipomopsis rubra KANU00242224
1272             Ipomopsis rubra                                     <NA>
1273             Ipomopsis rubra               Ipomopsis rubra KANU352624
1274             Ipomopsis rubra             Ipomopsis rubra KANU00242224
1275             Ipomopsis rubra             Ipomopsis rubra KANU00242224
1276           Ipomopsis sonorae              Ipomopsis sonorae ASU252692
1277           Ipomopsis sonorae              Ipomopsis sonorae ASU188835
1278           Ipomopsis sonorae              Ipomopsis sonorae ASU188835
1279           Ipomopsis sonorae              Ipomopsis sonorae ASU252692
1280           Ipomopsis sonorae                                     <NA>
1281           Ipomopsis sonorae              Ipomopsis sonorae ASU188835
1282        Ipomopsis tenuifolia        Ipomopsis tenuifolia KANU00237806
1283        Ipomopsis tenuifolia        Ipomopsis tenuifolia KANU00237806
1284         Ipomopsis tenuituba            Ipomopsis tenuituba ASU281978
1285         Ipomopsis tenuituba            Ipomopsis tenuituba ASU208786
1286         Ipomopsis tenuituba            Ipomopsis tenuituba ASU281978
1287         Ipomopsis tenuituba            Ipomopsis tenuituba ASU208786
1288         Ipomopsis tenuituba            Ipomopsis tenuituba ASU208786
1289         Ipomopsis tenuituba                                     <NA>
1290         Ipomopsis tenuituba            Ipomopsis tenuituba ASU281978
1291          Ipomopsis thurberi             Ipomopsis thurberi ASU193998
1292          Ipomopsis thurberi             Ipomopsis thurberi ASU193998
1293          Ipomopsis thurberi          Ipomopsis thurberi KANU00242276
1294          Ipomopsis thurberi             Ipomopsis thurberi ASU262383
1295          Ipomopsis thurberi                                     <NA>
1296          Ipomopsis thurberi            Ipomopsis thurberi KANU355538
1297          Ipomopsis thurberi             Ipomopsis thurberi ASU262383
1298          Ipomopsis thurberi            Ipomopsis thurberi KANU355538
1299          Ipomopsis thurberi             Ipomopsis thurberi ASU193998
1300          Ipomopsis thurberi            Ipomopsis thurberi KSC0126411
1301          Ipomopsis thurberi            Ipomopsis thurberi KANU355538
1302          Ipomopsis thurberi          Ipomopsis thurberi KANU00242276
1303          Ipomopsis thurberi             Ipomopsis thurberi ASU262383
1304          Ipomopsis thurberi            Ipomopsis thurberi KSC0126411
1305          Ipomopsis thurberi            Ipomopsis thurberi KSC0126411
1306          Ipomopsis thurberi          Ipomopsis thurberi KANU00242276
1307        Ipomopsis tridactyla            Ipomopsis tridactyla MO991093
1308        Ipomopsis tridactyla           Ipomopsis tridactyla MO1003493
1309        Ipomopsis tridactyla            Ipomopsis tridactyla MO991093
1310        Ipomopsis tridactyla           Ipomopsis tridactyla MO1003493
1311        Ipomopsis tridactyla            Ipomopsis tridactyla MO991093
1312        Ipomopsis tridactyla           Ipomopsis tridactyla MO1003493
1313        Ipomopsis tridactyla                                     <NA>
1314          Ipomopsis wrightii             Ipomopsis wrightii MO3217941
1315          Ipomopsis wrightii             Ipomopsis wrightii ASU246138
1316          Ipomopsis wrightii                                     <NA>
1317          Ipomopsis wrightii             Ipomopsis wrightii MO3217941
1318          Ipomopsis wrightii             Ipomopsis wrightii MO4003571
1319          Ipomopsis wrightii             Ipomopsis wrightii ASU247650
1320          Ipomopsis wrightii             Ipomopsis wrightii MO3217941
1321          Ipomopsis wrightii             Ipomopsis wrightii ASU246138
1322          Ipomopsis wrightii             Ipomopsis wrightii ASU247650
1323          Ipomopsis wrightii             Ipomopsis wrightii MO4003571
1324          Ipomopsis wrightii             Ipomopsis wrightii MO4003571
1325          Ipomopsis wrightii             Ipomopsis wrightii ASU246138
1326          Ipomopsis wrightii             Ipomopsis wrightii ASU247650
1327         Langloisia punctata         Langloisia punctata KANU00245452
1328         Langloisia punctata         Langloisia punctata KANU00245454
1329         Langloisia punctata           Langloisia punctata KANU358618
1330         Langloisia punctata           Langloisia punctata KANU358618
1331         Langloisia punctata         Langloisia punctata KANU00245454
1332         Langloisia punctata           Langloisia punctata KANU358618
1333         Langloisia punctata         Langloisia punctata KANU00245452
1334         Langloisia punctata         Langloisia punctata KANU00245454
1335         Langloisia punctata         Langloisia punctata KANU00245452
1336         Langloisia punctata                                     <NA>
1337      Langloisia setosissima                                     <NA>
1338      Langloisia setosissima      Langloisia setosissima KANU00245459
1339      Langloisia setosissima        Langloisia setosissima KSC0126435
1340      Langloisia setosissima        Langloisia setosissima KSC0126435
1341      Langloisia setosissima      Langloisia setosissima KANU00245459
1342      Langloisia setosissima      Langloisia setosissima KANU00245460
1343      Langloisia setosissima        Langloisia setosissima KANU361666
1344      Langloisia setosissima        Langloisia setosissima KSC0126431
1345      Langloisia setosissima        Langloisia setosissima KANU361666
1346      Langloisia setosissima        Langloisia setosissima KSC0126435
1347      Langloisia setosissima      Langloisia setosissima KANU00245460
1348      Langloisia setosissima      Langloisia setosissima KANU00245460
1349      Langloisia setosissima      Langloisia setosissima KANU00245459
1350      Langloisia setosissima        Langloisia setosissima KANU361666
1351       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00319369
1352       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00237799
1353       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00319369
1354       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00319415
1355       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00237799
1356       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00319415
1357       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00319415
1358       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00237799
1359       Lathrocasis tenerrima                                     <NA>
1360       Lathrocasis tenerrima       Lathrocasis tenerrima KANU00319369
1361      Leptosiphon acicularis         Leptosiphon acicularis MO1231089
1362      Leptosiphon acicularis         Leptosiphon acicularis MO1231089
1363      Leptosiphon acicularis         Leptosiphon acicularis MO1231089
1364      Leptosiphon acicularis         Leptosiphon acicularis MO4210887
1365      Leptosiphon acicularis         Leptosiphon acicularis MO4210887
1366      Leptosiphon acicularis         Leptosiphon acicularis MO4271541
1367      Leptosiphon acicularis                                     <NA>
1368      Leptosiphon acicularis         Leptosiphon acicularis MO4210887
1369      Leptosiphon acicularis         Leptosiphon acicularis MO4271541
1370      Leptosiphon acicularis         Leptosiphon acicularis MO4271541
1371        Leptosiphon ambiguus        Leptosiphon ambiguus KANU00249016
1372        Leptosiphon ambiguus        Leptosiphon ambiguus KANU00249016
1373        Leptosiphon ambiguus        Leptosiphon ambiguus KANU00249016
1374        Leptosiphon ambiguus        Leptosiphon ambiguus KANU00249017
1375        Leptosiphon ambiguus                                     <NA>
1376        Leptosiphon ambiguus        Leptosiphon ambiguus KANU00249017
1377        Leptosiphon ambiguus        Leptosiphon ambiguus KANU00249017
1378       Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249021
1379       Leptosiphon, rosaceus         Leptosiphon, rosaceus KANU365874
1380       Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249023
1381       Leptosiphon, rosaceus                                     <NA>
1382       Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249021
1383       Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249023
1384       Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249021
1385       Leptosiphon, rosaceus         Leptosiphon, rosaceus KANU365874
1386       Leptosiphon, rosaceus         Leptosiphon, rosaceus KANU365874
1387       Leptosiphon, rosaceus       Leptosiphon, rosaceus KANU00249023
1388          Leptosiphon aureus          Leptosiphon aureus KANU00249028
1389          Leptosiphon aureus          Leptosiphon aureus KANU00249028
1390          Leptosiphon aureus          Leptosiphon aureus KANU00249029
1391          Leptosiphon aureus          Leptosiphon aureus KANU00249028
1392          Leptosiphon aureus                                     <NA>
1393          Leptosiphon aureus          Leptosiphon aureus KANU00249029
1394          Leptosiphon aureus          Leptosiphon aureus KANU00249029
1395         Leptosiphon bicolor         Leptosiphon bicolor KANU00249042
1396         Leptosiphon bicolor         Leptosiphon bicolor KANU00305479
1397         Leptosiphon bicolor                                     <NA>
1398         Leptosiphon bicolor         Leptosiphon bicolor KANU00249042
1399         Leptosiphon bicolor         Leptosiphon bicolor KANU00249041
1400         Leptosiphon bicolor         Leptosiphon bicolor KANU00249039
1401         Leptosiphon bicolor         Leptosiphon bicolor KANU00249039
1402         Leptosiphon bicolor         Leptosiphon bicolor KANU00249039
1403         Leptosiphon bicolor         Leptosiphon bicolor KANU00249041
1404         Leptosiphon bicolor         Leptosiphon bicolor KANU00249041
1405         Leptosiphon bicolor         Leptosiphon bicolor KANU00249042
1406         Leptosiphon bicolor         Leptosiphon bicolor KANU00305479
1407         Leptosiphon bicolor         Leptosiphon bicolor KANU00305479
1408       Leptosiphon bolanderi         Leptosiphon bolanderi KANU365923
1409       Leptosiphon bolanderi         Leptosiphon bolanderi KANU365923
1410       Leptosiphon bolanderi         Leptosiphon bolanderi KANU365923
1411       Leptosiphon bolanderi                                     <NA>
1412      Leptosiphon breviculus      Leptosiphon breviculus KANU00249047
1413      Leptosiphon breviculus      Leptosiphon breviculus KANU00249047
1414      Leptosiphon breviculus      Leptosiphon breviculus KANU00249047
1415      Leptosiphon breviculus                                     <NA>
1416        Leptosiphon ciliatus          Leptosiphon ciliatus KSC0126247
1417        Leptosiphon ciliatus             Leptosiphon ciliatus UF72289
1418        Leptosiphon ciliatus           Leptosiphon ciliatus FLAS72289
1419        Leptosiphon ciliatus        Leptosiphon ciliatus KANU00249048
1420        Leptosiphon ciliatus             Leptosiphon ciliatus UF72289
1421        Leptosiphon ciliatus          Leptosiphon ciliatus KSC0126248
1422        Leptosiphon ciliatus          Leptosiphon ciliatus KSC0126247
1423        Leptosiphon ciliatus          Leptosiphon ciliatus KSC0126248
1424        Leptosiphon ciliatus          Leptosiphon ciliatus KSC0126247
1425        Leptosiphon ciliatus        Leptosiphon ciliatus KANU00249048
1426        Leptosiphon ciliatus                                     <NA>
1427        Leptosiphon ciliatus          Leptosiphon ciliatus KSC0126248
1428        Leptosiphon ciliatus        Leptosiphon ciliatus KANU00249048
1429        Leptosiphon ciliatus           Leptosiphon ciliatus FLAS72289
1430        Leptosiphon ciliatus           Leptosiphon ciliatus FLAS72289
1431        Leptosiphon ciliatus             Leptosiphon ciliatus UF72289
1432         Leptosiphon filipes         Leptosiphon filipes KANU00249063
1433         Leptosiphon filipes         Leptosiphon filipes KANU00249062
1434         Leptosiphon filipes         Leptosiphon filipes KANU00249062
1435         Leptosiphon filipes                                     <NA>
1436         Leptosiphon filipes         Leptosiphon filipes KANU00249063
1437         Leptosiphon filipes         Leptosiphon filipes KANU00249062
1438         Leptosiphon filipes         Leptosiphon filipes KANU00249063
1439     Leptosiphon floribundus       Leptosiphon floribundus KSC0126268
1440     Leptosiphon floribundus       Leptosiphon floribundus KSC0126268
1441     Leptosiphon floribundus       Leptosiphon floribundus KSC0126268
1442     Leptosiphon floribundus                                     <NA>
1443    Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249065
1444    Leptosiphon grandiflorus                                     <NA>
1445    Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249065
1446    Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249066
1447    Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249065
1448    Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249066
1449    Leptosiphon grandiflorus    Leptosiphon grandiflorus KANU00249066
1450      Leptosiphon harknessii        Leptosiphon harknessii KANU365232
1451      Leptosiphon harknessii        Leptosiphon harknessii KANU365215
1452      Leptosiphon harknessii                                     <NA>
1453      Leptosiphon harknessii        Leptosiphon harknessii KANU365215
1454      Leptosiphon harknessii        Leptosiphon harknessii KANU365232
1455      Leptosiphon harknessii        Leptosiphon harknessii KANU365232
1456      Leptosiphon harknessii        Leptosiphon harknessii KANU365215
1457        Leptosiphon jepsonii                                     <NA>
1458        Leptosiphon jepsonii           Leptosiphon jepsonii MO1150737
1459        Leptosiphon jepsonii           Leptosiphon jepsonii MO1150737
1460        Leptosiphon jepsonii          Leptosiphon jepsonii MO05056021
1461        Leptosiphon jepsonii           Leptosiphon jepsonii MO1150737
1462        Leptosiphon jepsonii          Leptosiphon jepsonii MO05056021
1463        Leptosiphon jepsonii          Leptosiphon jepsonii MO05056021
1464        Leptosiphon lemmonii           Leptosiphon lemmonii MO3641445
1465        Leptosiphon lemmonii           Leptosiphon lemmonii MO2753572
1466        Leptosiphon lemmonii                                     <NA>
1467        Leptosiphon lemmonii           Leptosiphon lemmonii MO3641444
1468        Leptosiphon lemmonii           Leptosiphon lemmonii MO3641445
1469        Leptosiphon lemmonii           Leptosiphon lemmonii MO2753572
1470        Leptosiphon lemmonii           Leptosiphon lemmonii MO3641444
1471        Leptosiphon lemmonii           Leptosiphon lemmonii MO3641444
1472        Leptosiphon lemmonii           Leptosiphon lemmonii MO3641445
1473        Leptosiphon lemmonii           Leptosiphon lemmonii MO2753572
1474      Leptosiphon liniflorus      Leptosiphon liniflorus KANU00249080
1475      Leptosiphon liniflorus      Leptosiphon liniflorus KANU00249080
1476      Leptosiphon liniflorus                                     <NA>
1477      Leptosiphon liniflorus      Leptosiphon liniflorus KANU00249080
1478    Leptosiphon mariposianus                                     <NA>
1479    Leptosiphon mariposianus      Leptosiphon mariposianus KSC0126462
1480    Leptosiphon mariposianus      Leptosiphon mariposianus KSC0126443
1481    Leptosiphon mariposianus      Leptosiphon mariposianus KSC0126443
1482    Leptosiphon mariposianus      Leptosiphon mariposianus KSC0126443
1483    Leptosiphon mariposianus      Leptosiphon mariposianus KSC0126462
1484    Leptosiphon mariposianus      Leptosiphon mariposianus KSC0126462
1485        Leptosiphon montanus        Leptosiphon montanus KANU00249083
1486        Leptosiphon montanus        Leptosiphon montanus KANU00249095
1487        Leptosiphon montanus        Leptosiphon montanus KANU00249095
1488        Leptosiphon montanus        Leptosiphon montanus KANU00249095
1489        Leptosiphon montanus        Leptosiphon montanus KANU00249083
1490        Leptosiphon montanus        Leptosiphon montanus KANU00249083
1491        Leptosiphon montanus                                     <NA>
1492         Leptosiphon nudatus         Leptosiphon nudatus KANU00249096
1493         Leptosiphon nudatus                                     <NA>
1494         Leptosiphon nudatus         Leptosiphon nudatus KANU00249096
1495         Leptosiphon nudatus         Leptosiphon nudatus KANU00249096
1496       Leptosiphon nuttallii            Leptosiphon nuttallii UF71467
1497       Leptosiphon nuttallii            Leptosiphon nuttallii UF71467
1498       Leptosiphon nuttallii         Leptosiphon nuttallii KANU365092
1499       Leptosiphon nuttallii       Leptosiphon nuttallii KANU00249084
1500       Leptosiphon nuttallii            Leptosiphon nuttallii UF71467
1501       Leptosiphon nuttallii         Leptosiphon nuttallii KANU365092
1502       Leptosiphon nuttallii       Leptosiphon nuttallii KANU00249084
1503       Leptosiphon nuttallii       Leptosiphon nuttallii KANU00249084
1504       Leptosiphon nuttallii            Leptosiphon nuttallii UF85671
1505       Leptosiphon nuttallii                                     <NA>
1506       Leptosiphon nuttallii            Leptosiphon nuttallii UF85671
1507       Leptosiphon nuttallii         Leptosiphon nuttallii KANU365092
1508       Leptosiphon nuttallii            Leptosiphon nuttallii UF85671
1509   Leptosiphon oblanceolatus   Leptosiphon oblanceolatus KANU00249097
1510   Leptosiphon oblanceolatus   Leptosiphon oblanceolatus KANU00249097
1511   Leptosiphon oblanceolatus   Leptosiphon oblanceolatus KANU00249097
1512   Leptosiphon oblanceolatus                                     <NA>
1513    Leptosiphon pachyphyllus                                     <NA>
1514    Leptosiphon pachyphyllus       Leptosiphon pachyphyllus MO2125655
1515    Leptosiphon pachyphyllus       Leptosiphon pachyphyllus MO2125652
1516    Leptosiphon pachyphyllus       Leptosiphon pachyphyllus MO2125655
1517    Leptosiphon pachyphyllus       Leptosiphon pachyphyllus MO2125655
1518    Leptosiphon pachyphyllus       Leptosiphon pachyphyllus MO2125652
1519    Leptosiphon pachyphyllus       Leptosiphon pachyphyllus MO2125652
1520     Leptosiphon parviflorus     Leptosiphon parviflorus KANU00237584
1521     Leptosiphon parviflorus     Leptosiphon parviflorus KANU00249024
1522     Leptosiphon parviflorus     Leptosiphon parviflorus KANU00249024
1523     Leptosiphon parviflorus                                     <NA>
1524     Leptosiphon parviflorus     Leptosiphon parviflorus KANU00237584
1525     Leptosiphon parviflorus     Leptosiphon parviflorus KANU00249024
1526     Leptosiphon parviflorus     Leptosiphon parviflorus KANU00237584
1527 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00315954
1528 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00249105
1529 Leptosiphon septentrionalis   Leptosiphon septentrionalis KANU365812
1530 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00249105
1531 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00319340
1532 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00315954
1533 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00319340
1534 Leptosiphon septentrionalis                                     <NA>
1535 Leptosiphon septentrionalis   Leptosiphon septentrionalis KANU365812
1536 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00319340
1537 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00249105
1538 Leptosiphon septentrionalis   Leptosiphon septentrionalis KANU365812
1539 Leptosiphon septentrionalis Leptosiphon septentrionalis KANU00315954
1540            Linanthus bellus             Linanthus bellus RSABG341957
1541            Linanthus bellus                                     <NA>
1542            Linanthus bellus             Linanthus bellus RSABG344309
1543            Linanthus bellus             Linanthus bellus RSABG341957
1544            Linanthus bellus             Linanthus bellus RSABG341957
1545            Linanthus bellus             Linanthus bellus RSABG344309
1546            Linanthus bellus             Linanthus bellus RSABG344309
1547         Linanthus bigelovii         Linanthus bigelovii KANU00249043
1548         Linanthus bigelovii         Linanthus bigelovii KANU00249043
1549         Linanthus bigelovii           Linanthus bigelovii KANU357542
1550         Linanthus bigelovii           Linanthus bigelovii KANU357542
1551         Linanthus bigelovii           Linanthus bigelovii KANU357542
1552         Linanthus bigelovii                                     <NA>
1553       Linanthus caespitosus       Linanthus caespitosus KANU00316297
1554       Linanthus caespitosus       Linanthus caespitosus KANU00248048
1555       Linanthus caespitosus       Linanthus caespitosus KANU00308733
1556       Linanthus caespitosus                                     <NA>
1557       Linanthus caespitosus       Linanthus caespitosus KANU00316297
1558       Linanthus caespitosus       Linanthus caespitosus KANU00248048
1559       Linanthus caespitosus       Linanthus caespitosus KANU00308733
1560       Linanthus caespitosus       Linanthus caespitosus KANU00316297
1561       Linanthus caespitosus       Linanthus caespitosus KANU00308733
1562       Linanthus caespitosus       Linanthus caespitosus KANU00248048
1563      Linanthus californicus                                     <NA>
1564      Linanthus californicus        Linanthus californicus KSC0126439
1565      Linanthus californicus        Linanthus californicus KSC0126439
1566      Linanthus californicus      Linanthus californicus KANU00248052
1567      Linanthus californicus         Linanthus californicus MO5333712
1568      Linanthus californicus         Linanthus californicus MO3193468
1569      Linanthus californicus         Linanthus californicus MO5333712
1570      Linanthus californicus         Linanthus californicus MO5333712
1571      Linanthus californicus           Linanthus californicus 5206791
1572      Linanthus californicus      Linanthus californicus KANU00316478
1573      Linanthus californicus      Linanthus californicus KANU00248052
1574      Linanthus californicus           Linanthus californicus 5206791
1575      Linanthus californicus           Linanthus californicus 5206791
1576      Linanthus californicus         Linanthus californicus MO3193468
1577      Linanthus californicus         Linanthus californicus MO3193468
1578      Linanthus californicus        Linanthus californicus KSC0126439
1579      Linanthus californicus      Linanthus californicus KANU00316478
1580      Linanthus californicus      Linanthus californicus KANU00316478
1581      Linanthus californicus      Linanthus californicus KANU00248052
1582      Linanthus campanulatus        Linanthus campanulatus KANU363759
1583      Linanthus campanulatus         Linanthus campanulatus FLAS39342
1584      Linanthus campanulatus        Linanthus campanulatus KANU363759
1585      Linanthus campanulatus         Linanthus campanulatus FLAS39342
1586      Linanthus campanulatus        Linanthus campanulatus KANU363759
1587      Linanthus campanulatus      Linanthus campanulatus KANU00237602
1588      Linanthus campanulatus                                     <NA>
1589      Linanthus campanulatus        Linanthus campanulatus KANU363636
1590      Linanthus campanulatus      Linanthus campanulatus KANU00237602
1591      Linanthus campanulatus         Linanthus campanulatus FLAS39342
1592      Linanthus campanulatus        Linanthus campanulatus KANU363636
1593      Linanthus campanulatus      Linanthus campanulatus KANU00237602
1594      Linanthus campanulatus        Linanthus campanulatus KANU363636
1595         Linanthus concinnus          Linanthus concinnus RSABG677724
1596         Linanthus concinnus          Linanthus concinnus RSABG778878
1597         Linanthus concinnus          Linanthus concinnus RSABG677724
1598         Linanthus concinnus                                     <NA>
1599         Linanthus concinnus          Linanthus concinnus RSABG677724
1600         Linanthus concinnus          Linanthus concinnus RSABG677724
1601         Linanthus concinnus          Linanthus concinnus RSABG778878
1602          Linanthus demissus            Linanthus demissus KANU363543
1603          Linanthus demissus            Linanthus demissus KANU363543
1604          Linanthus demissus                                     <NA>
1605          Linanthus demissus          Linanthus demissus KANU00249055
1606          Linanthus demissus          Linanthus demissus KANU00249055
1607          Linanthus demissus          Linanthus demissus KANU00249055
1608          Linanthus demissus            Linanthus demissus KANU363543
1609     Linanthus dianthiflorus                                     <NA>
1610     Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126232
1611     Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126239
1612     Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126239
1613     Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126232
1614     Linanthus dianthiflorus     Linanthus dianthiflorus KANU00249056
1615     Linanthus dianthiflorus     Linanthus dianthiflorus KANU00249056
1616     Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126239
1617     Linanthus dianthiflorus     Linanthus dianthiflorus KANU00249056
1618     Linanthus dianthiflorus       Linanthus dianthiflorus KSC0126232
1619        Linanthus dichotomus          Linanthus dichotomus KANU363552
1620        Linanthus dichotomus          Linanthus dichotomus KANU363552
1621        Linanthus dichotomus          Linanthus dichotomus KANU363504
1622        Linanthus dichotomus          Linanthus dichotomus KSC0126234
1623        Linanthus dichotomus          Linanthus dichotomus KSC0126234
1624        Linanthus dichotomus          Linanthus dichotomus KSC0126234
1625        Linanthus dichotomus          Linanthus dichotomus KANU363519
1626        Linanthus dichotomus          Linanthus dichotomus KANU363519
1627        Linanthus dichotomus          Linanthus dichotomus KANU363504
1628        Linanthus dichotomus          Linanthus dichotomus KANU363504
1629        Linanthus dichotomus          Linanthus dichotomus KANU363552
1630        Linanthus dichotomus          Linanthus dichotomus KANU363519
1631        Linanthus dichotomus                                     <NA>
1632        Linanthus filiformis            Linanthus filiformis UF144304
1633        Linanthus filiformis        Linanthus filiformis KANU00237639
1634        Linanthus filiformis        Linanthus filiformis KANU00237639
1635        Linanthus filiformis          Linanthus filiformis FLAS144304
1636        Linanthus filiformis        Linanthus filiformis KANU00237639
1637        Linanthus filiformis            Linanthus filiformis UF144304
1638        Linanthus filiformis          Linanthus filiformis FLAS144304
1639        Linanthus filiformis                                     <NA>
1640        Linanthus filiformis            Linanthus filiformis UF144304
1641        Linanthus filiformis          Linanthus filiformis FLAS144304
1642         Linanthus inyoensis                                     <NA>
1643         Linanthus inyoensis            Linanthus inyoensis MO2754158
1644         Linanthus inyoensis            Linanthus inyoensis MO1660045
1645         Linanthus inyoensis            Linanthus inyoensis MO1660045
1646         Linanthus inyoensis            Linanthus inyoensis MO1660045
1647         Linanthus inyoensis            Linanthus inyoensis MO2754158
1648         Linanthus inyoensis            Linanthus inyoensis MO3500069
1649         Linanthus inyoensis            Linanthus inyoensis MO3500069
1650         Linanthus inyoensis            Linanthus inyoensis MO3500069
1651         Linanthus inyoensis            Linanthus inyoensis MO2754158
1652           Linanthus jonesii                                     <NA>
1653           Linanthus jonesii           Linanthus jonesii KANU00249078
1654           Linanthus jonesii           Linanthus jonesii KANU00249078
1655           Linanthus jonesii           Linanthus jonesii KANU00249078
1656             Linanthus laxus             Linanthus laxus KANU00249079
1657             Linanthus laxus             Linanthus laxus KANU00249079
1658             Linanthus laxus                                     <NA>
1659             Linanthus laxus             Linanthus laxus KANU00249079
1660         Linanthus maculatus                                     <NA>
1661         Linanthus maculatus          Linanthus maculatus RSABG705184
1662         Linanthus maculatus          Linanthus maculatus RSABG705184
1663         Linanthus maculatus          Linanthus maculatus RSABG705184
1664          Linanthus melingii             Linanthus melingii MO2369606
1665          Linanthus melingii                                     <NA>
1666          Linanthus melingii             Linanthus melingii MO2369606
1667          Linanthus melingii             Linanthus melingii MO2369606
1668           Linanthus, cuttii              Linanthus, cuttii MO2371408
1669           Linanthus, cuttii              Linanthus, cuttii MO2371408
1670           Linanthus, cuttii                                     <NA>
1671           Linanthus, cuttii              Linanthus, cuttii MO2371408
1672           Linanthus parryae             Linanthus parryae KSC0126158
1673           Linanthus parryae                                     <NA>
1674           Linanthus parryae           Linanthus parryae KANU00249098
1675           Linanthus parryae           Linanthus parryae KANU00249098
1676           Linanthus parryae           Linanthus parryae KANU00249098
1677           Linanthus parryae             Linanthus parryae KSC0126158
1678           Linanthus parryae             Linanthus parryae KSC0126158
1679           Linanthus pungens           Linanthus pungens KANU00248059
1680           Linanthus pungens           Linanthus pungens KANU00248059
1681           Linanthus pungens           Linanthus pungens KANU00248060
1682           Linanthus pungens           Linanthus pungens KANU00248060
1683           Linanthus pungens                                     <NA>
1684           Linanthus pungens           Linanthus pungens KANU00248061
1685           Linanthus pungens           Linanthus pungens KANU00248061
1686           Linanthus pungens           Linanthus pungens KANU00248060
1687           Linanthus pungens           Linanthus pungens KANU00248061
1688           Linanthus pungens           Linanthus pungens KANU00248059
1689          Linanthus uncialis           Linanthus uncialis RSABG490647
1690          Linanthus uncialis           Linanthus uncialis RSABG490647
1691          Linanthus uncialis                                     <NA>
1692          Linanthus uncialis           Linanthus uncialis RSABG490647
1693          Linanthus watsonii             Linanthus watsonii MO2043525
1694          Linanthus watsonii             Linanthus watsonii MO2043525
1695          Linanthus watsonii             Linanthus watsonii MO2043525
1696          Linanthus watsonii             Linanthus watsonii MO1172827
1697          Linanthus watsonii             Linanthus watsonii MO2753348
1698          Linanthus watsonii             Linanthus watsonii MO1172827
1699          Linanthus watsonii             Linanthus watsonii MO2753349
1700          Linanthus watsonii                                     <NA>
1701          Linanthus watsonii             Linanthus watsonii MO1172827
1702          Linanthus watsonii             Linanthus watsonii MO2753349
1703          Linanthus watsonii             Linanthus watsonii MO2753348
1704          Linanthus watsonii             Linanthus watsonii MO2753348
1705          Linanthus watsonii             Linanthus watsonii MO2753349
1706         Loeselia amplectens            Loeselia amplectens MO3758588
1707         Loeselia amplectens            Loeselia amplectens MO4074472
1708         Loeselia amplectens            Loeselia amplectens MO4074472
1709         Loeselia amplectens                                     <NA>
1710         Loeselia amplectens            Loeselia amplectens MO3758588
1711         Loeselia amplectens            Loeselia amplectens MO4074472
1712         Loeselia amplectens            Loeselia amplectens MO3758588
1713            Loeselia ciliata               Loeselia ciliata MO5320807
1714            Loeselia ciliata               Loeselia ciliata MO5320811
1715            Loeselia ciliata               Loeselia ciliata MO5320807
1716            Loeselia ciliata               Loeselia ciliata MO5320813
1717            Loeselia ciliata                                     <NA>
1718            Loeselia ciliata               Loeselia ciliata MO5320813
1719            Loeselia ciliata               Loeselia ciliata MO5320811
1720            Loeselia ciliata               Loeselia ciliata MO5320811
1721            Loeselia ciliata               Loeselia ciliata MO5320813
1722           Loeselia coerulea             Loeselia coerulea MO04470707
1723           Loeselia coerulea                                     <NA>
1724           Loeselia coerulea              Loeselia coerulea MO2388700
1725           Loeselia coerulea              Loeselia coerulea MO2388700
1726           Loeselia coerulea              Loeselia coerulea MO3464356
1727           Loeselia coerulea              Loeselia coerulea MO3464356
1728           Loeselia coerulea              Loeselia coerulea MO3464356
1729           Loeselia coerulea             Loeselia coerulea MO04470707
1730           Loeselia coerulea             Loeselia coerulea MO04470707
1731           Loeselia coerulea              Loeselia coerulea MO2388700
1732         Loeselia glandulosa         Loeselia glandulosa KANU00256195
1733         Loeselia glandulosa                                     <NA>
1734         Loeselia glandulosa         Loeselia glandulosa KANU00256195
1735         Loeselia glandulosa         Loeselia glandulosa KANU00256194
1736         Loeselia glandulosa         Loeselia glandulosa KANU00256194
1737         Loeselia glandulosa         Loeselia glandulosa KANU00256195
1738         Loeselia glandulosa         Loeselia glandulosa KANU00256194
1739        Loeselia grandiflora           Loeselia grandiflora MO1180165
1740        Loeselia grandiflora           Loeselia grandiflora MO1208113
1741        Loeselia grandiflora                                     <NA>
1742        Loeselia grandiflora           Loeselia grandiflora MO1180165
1743        Loeselia grandiflora           Loeselia grandiflora MO1208113
1744        Loeselia grandiflora           Loeselia grandiflora MO1180165
1745        Loeselia grandiflora           Loeselia grandiflora MO1208113
1746            Loeselia greggii             Loeselia greggii RSABG683294
1747            Loeselia greggii               Loeselia greggii MO3615225
1748            Loeselia greggii               Loeselia greggii MO3615222
1749            Loeselia greggii               Loeselia greggii MO3615223
1750            Loeselia greggii             Loeselia greggii RSABG637627
1751            Loeselia greggii               Loeselia greggii MO2753371
1752            Loeselia greggii               Loeselia greggii MO3615222
1753            Loeselia greggii               Loeselia greggii MO3615223
1754            Loeselia greggii             Loeselia greggii RSABG637627
1755            Loeselia greggii               Loeselia greggii MO3615225
1756            Loeselia greggii               Loeselia greggii MO2753371
1757            Loeselia greggii             Loeselia greggii RSABG637627
1758            Loeselia greggii                                     <NA>
1759            Loeselia greggii             Loeselia greggii RSABG683294
1760            Loeselia greggii               Loeselia greggii MO2753371
1761            Loeselia greggii               Loeselia greggii MO3615225
1762        Loeselia involucrata           Loeselia involucrata MO5447714
1763        Loeselia involucrata           Loeselia involucrata MO3761529
1764        Loeselia involucrata           Loeselia involucrata MO5447714
1765        Loeselia involucrata           Loeselia involucrata MO3761529
1766        Loeselia involucrata           Loeselia involucrata MO5447714
1767        Loeselia involucrata           Loeselia involucrata MO2753376
1768        Loeselia involucrata                                     <NA>
1769        Loeselia involucrata           Loeselia involucrata MO2753376
1770        Loeselia involucrata           Loeselia involucrata MO2753376
1771           Loeselia mexicana                                     <NA>
1772           Loeselia mexicana           Loeselia mexicana KANU00256196
1773           Loeselia mexicana           Loeselia mexicana KANU00256196
1774           Loeselia mexicana           Loeselia mexicana KANU00256196
1775             Loeselia pumila                Loeselia pumila MO2244971
1776             Loeselia pumila                Loeselia pumila MO3641392
1777             Loeselia pumila                Loeselia pumila MO3714055
1778             Loeselia pumila                                     <NA>
1779             Loeselia pumila                Loeselia pumila MO3641392
1780             Loeselia pumila                Loeselia pumila MO3641392
1781             Loeselia pumila              Loeselia pumila RSABG722878
1782             Loeselia pumila                Loeselia pumila MO2244971
1783             Loeselia pumila              Loeselia pumila RSABG722878
1784             Loeselia pumila                Loeselia pumila MO3714055
1785             Loeselia pumila              Loeselia pumila RSABG709301
1786             Loeselia pumila                Loeselia pumila MO3714055
1787             Loeselia pumila                Loeselia pumila MO2244971
1788             Loeselia pumila              Loeselia pumila RSABG722878
1789             Loeselia pumila              Loeselia pumila RSABG709301
1790             Loeselia pumila              Loeselia pumila RSABG709301
1791           Loeselia purpusii              Loeselia purpusii MO6197259
1792           Loeselia purpusii              Loeselia purpusii MO6197259
1793           Loeselia purpusii                                     <NA>
1794           Loeselia purpusii              Loeselia purpusii MO6197259
1795           Loeselia purpusii             Loeselia purpusii MO04470712
1796           Loeselia purpusii             Loeselia purpusii MO04470712
1797           Loeselia purpusii             Loeselia purpusii MO04470712
1798         Loeselia rzedowskii          Loeselia rzedowskii RSABG692006
1799         Loeselia rzedowskii          Loeselia rzedowskii RSABG692006
1800         Loeselia rzedowskii                                     <NA>
1801         Loeselia rzedowskii          Loeselia rzedowskii RSABG692006
1802        Loeselia spectabilis         Loeselia spectabilis RSABG688743
1803        Loeselia spectabilis         Loeselia spectabilis RSABG688743
1804        Loeselia spectabilis                                     <NA>
1805     Loeselia tancitaroensis                                     <NA>
1806     Loeselia tancitaroensis      Loeselia tancitaroensis RSABG688738
1807     Loeselia tancitaroensis      Loeselia tancitaroensis RSABG688738
1808     Loeselia tancitaroensis      Loeselia tancitaroensis RSABG688742
1809     Loeselia tancitaroensis      Loeselia tancitaroensis RSABG688738
1810     Loeseliastrum depressum      Loeseliastrum depressum RSABG503663
1811     Loeseliastrum depressum                                     <NA>
1812     Loeseliastrum depressum      Loeseliastrum depressum RSABG782466
1813     Loeseliastrum depressum      Loeseliastrum depressum RSABG782466
1814     Loeseliastrum depressum      Loeseliastrum depressum RSABG503663
1815     Loeseliastrum depressum      Loeseliastrum depressum RSABG782466
1816     Loeseliastrum depressum      Loeseliastrum depressum RSABG503663
1817    Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245450
1818    Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245450
1819    Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245450
1820    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126432
1821    Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245451
1822    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126432
1823    Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245451
1824    Loeseliastrum matthewsii    Loeseliastrum matthewsii KANU00245451
1825    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126436
1826    Loeseliastrum matthewsii                                     <NA>
1827    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126433
1828    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126436
1829    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126436
1830    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126432
1831    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126433
1832    Loeseliastrum matthewsii      Loeseliastrum matthewsii KSC0126433
1833      Loeseliastrum schottii      Loeseliastrum schottii KANU00245458
1834      Loeseliastrum schottii        Loeseliastrum schottii KANU363761
1835      Loeseliastrum schottii      Loeseliastrum schottii KANU00245455
1836      Loeseliastrum schottii      Loeseliastrum schottii KANU00245458
1837      Loeseliastrum schottii      Loeseliastrum schottii KANU00245458
1838      Loeseliastrum schottii      Loeseliastrum schottii KANU00245455
1839      Loeseliastrum schottii                                     <NA>
1840      Loeseliastrum schottii        Loeseliastrum schottii KANU363761
1841      Loeseliastrum schottii        Loeseliastrum schottii KANU363761
1842      Microgilia minutiflora      Microgilia minutiflora KANU00242198
1843      Microgilia minutiflora      Microgilia minutiflora KANU00242198
1844      Microgilia minutiflora                                     <NA>
1845      Microgilia minutiflora      Microgilia minutiflora KANU00242198
1846        Microsteris gracilis          Microsteris gracilis KSC0126260
1847        Microsteris gracilis        Microsteris gracilis KANU00265609
1848        Microsteris gracilis        Microsteris gracilis KANU00265630
1849        Microsteris gracilis                                     <NA>
1850        Microsteris gracilis          Microsteris gracilis KSC0126256
1851        Microsteris gracilis          Microsteris gracilis KSC0126260
1852        Microsteris gracilis          Microsteris gracilis KSC0126256
1853        Microsteris gracilis        Microsteris gracilis KANU00265630
1854        Microsteris gracilis          Microsteris gracilis KSC0126256
1855        Microsteris gracilis        Microsteris gracilis KANU00312371
1856        Microsteris gracilis        Microsteris gracilis KANU00312371
1857        Microsteris gracilis        Microsteris gracilis KANU00312371
1858        Microsteris gracilis        Microsteris gracilis KANU00265609
1859        Microsteris gracilis        Microsteris gracilis KANU00265630
1860        Microsteris gracilis        Microsteris gracilis KANU00265609
1861    Navarretia atractyloides    Navarretia atractyloides KANU00257719
1862    Navarretia atractyloides    Navarretia atractyloides KANU00257719
1863    Navarretia atractyloides                                     <NA>
1864    Navarretia atractyloides    Navarretia atractyloides KANU00257719
1865          Navarretia breweri          Navarretia breweri KANU00319191
1866          Navarretia breweri          Navarretia breweri KANU00319191
1867          Navarretia breweri          Navarretia breweri KANU00257723
1868          Navarretia breweri                                     <NA>
1869          Navarretia breweri          Navarretia breweri KANU00257723
1870          Navarretia breweri          Navarretia breweri KANU00319191
1871       Navarretia capillaris         Navarretia capillaris KANU363792
1872       Navarretia capillaris         Navarretia capillaris KANU363792
1873       Navarretia capillaris         Navarretia capillaris KANU363792
1874       Navarretia capillaris                                     <NA>
1875       Navarretia capillaris       Navarretia capillaris KANU00237614
1876       Navarretia capillaris       Navarretia capillaris KANU00237614
1877       Navarretia capillaris       Navarretia capillaris KANU00237614
1878       Navarretia divaricata         Navarretia divaricata KSC0126231
1879       Navarretia divaricata         Navarretia divaricata KSC0126231
1880       Navarretia divaricata       Navarretia divaricata KANU00257729
1881       Navarretia divaricata                                     <NA>
1882       Navarretia divaricata         Navarretia divaricata KSC0126231
1883       Navarretia divaricata       Navarretia divaricata KANU00257729
1884       Navarretia divaricata       Navarretia divaricata KANU00257729
1885       Navarretia filicaulis                                     <NA>
1886       Navarretia filicaulis         Navarretia filicaulis KSC0126272
1887       Navarretia filicaulis         Navarretia filicaulis KSC0126272
1888       Navarretia filicaulis         Navarretia filicaulis KSC0126272
1889         Navarretia fossalis         Navarretia fossalis KANU00257733
1890         Navarretia fossalis         Navarretia fossalis KANU00257733
1891         Navarretia fossalis                                     <NA>
1892         Navarretia fossalis         Navarretia fossalis KANU00257733
1893           Navarretia hamata           Navarretia hamata KANU00257734
1894           Navarretia hamata           Navarretia hamata KANU00257734
1895           Navarretia hamata                                     <NA>
1896       Navarretia heterandra          Navarretia heterandra MO4222432
1897       Navarretia heterandra                                     <NA>
1898       Navarretia heterandra          Navarretia heterandra MO4222432
1899       Navarretia heterandra          Navarretia heterandra MO4222432
1900       Navarretia heterodoxa       Navarretia heterodoxa KANU00257735
1901       Navarretia heterodoxa       Navarretia heterodoxa KANU00257735
1902       Navarretia heterodoxa       Navarretia heterodoxa KANU00257735
1903       Navarretia heterodoxa                                     <NA>
1904       Navarretia intertexta       Navarretia intertexta KANU00257741
1905       Navarretia intertexta                                     <NA>
1906       Navarretia intertexta       Navarretia intertexta KANU00257741
1907       Navarretia intertexta       Navarretia intertexta KANU00257741
1908          Navarretia jaredii             Navarretia jaredii MO5760682
1909         Navarretia jepsonii                                     <NA>
1910         Navarretia jepsonii         Navarretia jepsonii KANU00257757
1911         Navarretia jepsonii         Navarretia jepsonii KANU00257757
1912         Navarretia jepsonii         Navarretia jepsonii KANU00257757
1913         Navarretia leptalea            Navarretia leptalea FLAS72028
1914         Navarretia leptalea              Navarretia leptalea UF72028
1915         Navarretia leptalea         Navarretia leptalea KANU00237681
1916         Navarretia leptalea         Navarretia leptalea KANU00237681
1917         Navarretia leptalea            Navarretia leptalea FLAS72028
1918         Navarretia leptalea         Navarretia leptalea KANU00237682
1919         Navarretia leptalea              Navarretia leptalea UF72028
1920         Navarretia leptalea         Navarretia leptalea KANU00237682
1921         Navarretia leptalea                                     <NA>
1922         Navarretia leptalea              Navarretia leptalea UF72028
1923         Navarretia leptalea         Navarretia leptalea KANU00237682
1924         Navarretia leptalea         Navarretia leptalea KANU00237681
1925         Navarretia leptalea            Navarretia leptalea FLAS72028
1926       Navarretia mitracarpa           Navarretia mitracarpa 00257751
1927       Navarretia mitracarpa                                     <NA>
1928       Navarretia mitracarpa           Navarretia mitracarpa 00257751
1929       Navarretia mitracarpa           Navarretia mitracarpa 00257751
1930     Navarretia peninsularis        Navarretia peninsularis MO3206031
1931     Navarretia peninsularis                                     <NA>
1932     Navarretia peninsularis        Navarretia peninsularis MO3206031
1933     Navarretia peninsularis        Navarretia peninsularis MO3206031
1934        Navarretia pubescens        Navarretia pubescens KANU00329164
1935        Navarretia pubescens        Navarretia pubescens KANU00329164
1936        Navarretia pubescens        Navarretia pubescens KANU00329164
1937        Navarretia pubescens        Navarretia pubescens KANU00257768
1938        Navarretia pubescens        Navarretia pubescens KANU00257768
1939        Navarretia pubescens                                     <NA>
1940        Navarretia pubescens        Navarretia pubescens KANU00257768
1941         Navarretia rosulata                                     <NA>
1942         Navarretia rosulata         Navarretia rosulata KANU00257764
1943         Navarretia rosulata         Navarretia rosulata KANU00257764
1944         Navarretia rosulata         Navarretia rosulata KANU00257764
1945      Navarretia saximontana         Navarretia saximontana MO3651598
1946         Navarretia sinistra          Navarretia sinistra RSABG562595
1947         Navarretia sinistra         Navarretia sinistra KANU00237771
1948         Navarretia sinistra         Navarretia sinistra KANU00237771
1949         Navarretia sinistra          Navarretia sinistra RSABG562595
1950         Navarretia sinistra          Navarretia sinistra RSABG562595
1951         Navarretia sinistra          Navarretia sinistra RSABG780855
1952         Navarretia sinistra          Navarretia sinistra RSABG780855
1953         Navarretia sinistra         Navarretia sinistra KANU00237771
1954         Navarretia sinistra                                     <NA>
1955         Navarretia sinistra          Navarretia sinistra RSABG780855
1956        Navarretia squarrosa        Navarretia squarrosa KANU00257775
1957        Navarretia squarrosa        Navarretia squarrosa KANU00257775
1958        Navarretia squarrosa        Navarretia squarrosa KANU00257738
1959        Navarretia squarrosa        Navarretia squarrosa KANU00314368
1960        Navarretia squarrosa                                     <NA>
1961        Navarretia squarrosa        Navarretia squarrosa KANU00314368
1962        Navarretia squarrosa        Navarretia squarrosa KANU00314368
1963        Navarretia squarrosa        Navarretia squarrosa KANU00257738
1964        Navarretia squarrosa        Navarretia squarrosa KANU00257738
1965        Navarretia squarrosa        Navarretia squarrosa KANU00257737
1966        Navarretia squarrosa        Navarretia squarrosa KANU00257737
1967        Navarretia squarrosa        Navarretia squarrosa KANU00257775
1968       Navarretia subuligera          Navarretia subuligera MO1243591
1969       Navarretia subuligera         Navarretia subuligera MO05088307
1970       Navarretia subuligera          Navarretia subuligera MO1243591
1971       Navarretia subuligera         Navarretia subuligera MO05088307
1972       Navarretia subuligera                                     <NA>
1973         Navarretia tagetina                                     <NA>
1974         Navarretia tagetina           Navarretia tagetina KANU365377
1975         Navarretia tagetina           Navarretia tagetina KANU365366
1976         Navarretia tagetina           Navarretia tagetina KANU365366
1977         Navarretia tagetina           Navarretia tagetina KANU365377
1978         Navarretia tagetina           Navarretia tagetina KANU365366
1979         Navarretia tagetina           Navarretia tagetina KANU365377
1980        Navarretia viscidula        Navarretia viscidula KANU00257782
1981        Navarretia viscidula        Navarretia viscidula KANU00257782
1982        Navarretia viscidula        Navarretia viscidula KANU00257782
1983        Navarretia viscidula        Navarretia viscidula KANU00257781
1984        Navarretia viscidula        Navarretia viscidula KANU00257781
1985        Navarretia viscidula                                     <NA>
1986        Navarretia viscidula        Navarretia viscidula KANU00257781
1987              Phlox aculeata              Phlox aculeata KANU00265318
1988              Phlox aculeata              Phlox aculeata KANU00265317
1989              Phlox aculeata              Phlox aculeata KANU00265318
1990              Phlox aculeata              Phlox aculeata KANU00265317
1991              Phlox aculeata                                     <NA>
1992              Phlox aculeata              Phlox aculeata KANU00265318
1993              Phlox aculeata              Phlox aculeata KANU00265317
1994             Phlox adsurgens             Phlox adsurgens KANU00265319
1995             Phlox adsurgens                                     <NA>
1996             Phlox adsurgens             Phlox adsurgens KANU00265319
1997           Phlox alyssifolia           Phlox alyssifolia KANU00265321
1998           Phlox alyssifolia                                     <NA>
1999           Phlox alyssifolia           Phlox alyssifolia KANU00265321
2000           Phlox alyssifolia           Phlox alyssifolia KANU00265321
2001           Phlox alyssifolia           Phlox alyssifolia KANU00265336
2002           Phlox alyssifolia           Phlox alyssifolia KANU00265336
2003           Phlox alyssifolia           Phlox alyssifolia KANU00265336
2004                Phlox amoena                Phlox amoena KANU00265356
2005                Phlox amoena                  Phlox amoena KSC0129871
2006                Phlox amoena                Phlox amoena KANU00265356
2007                Phlox amoena                Phlox amoena KANU00265358
2008                Phlox amoena                  Phlox amoena KSC0129875
2009                Phlox amoena                  Phlox amoena KSC0129875
2010                Phlox amoena                Phlox amoena KANU00265360
2011                Phlox amoena                Phlox amoena KANU00265356
2012                Phlox amoena                  Phlox amoena KSC0129874
2013                Phlox amoena                  Phlox amoena KSC0129875
2014                Phlox amoena                  Phlox amoena KSC0129873
2015                Phlox amoena                  Phlox amoena KSC0129871
2016                Phlox amoena                Phlox amoena KANU00265360
2017                Phlox amoena                Phlox amoena KANU00265358
2018                Phlox amoena                                     <NA>
2019                Phlox amoena                  Phlox amoena KSC0129874
2020                Phlox amoena                Phlox amoena KANU00265360
2021                Phlox amoena                  Phlox amoena KSC0129874
2022                Phlox amoena                  Phlox amoena KSC0129871
2023                Phlox amoena                Phlox amoena KANU00265358
2024                Phlox amoena                  Phlox amoena KSC0129873
2025                Phlox amoena                  Phlox amoena KSC0129873
2026            Phlox amplifolia              Phlox amplifolia KSC0129890
2027            Phlox amplifolia              Phlox amplifolia KSC0129890
2028            Phlox amplifolia                                     <NA>
2029                Phlox, icola                Phlox, icola KANU00265362
2030                Phlox, icola                Phlox, icola KANU00265366
2031                Phlox, icola                                     <NA>
2032                Phlox, icola                Phlox, icola KANU00330422
2033                Phlox, icola                Phlox, icola KANU00265366
2034                Phlox, icola                Phlox, icola KANU00265362
2035                Phlox, icola                Phlox, icola KANU00265362
2036                Phlox, icola                Phlox, icola KANU00265366
2037                Phlox, icola                Phlox, icola KANU00330422
2038                Phlox, icola                Phlox, icola KANU00330422
2039         Phlox austromontana                                     <NA>
2040         Phlox austromontana         Phlox austromontana KANU00265433
2041         Phlox austromontana         Phlox austromontana KANU00325775
2042         Phlox austromontana         Phlox austromontana KANU00265433
2043         Phlox austromontana         Phlox austromontana KANU00325775
2044         Phlox austromontana         Phlox austromontana KANU00325775
2045         Phlox austromontana         Phlox austromontana KANU00265432
2046         Phlox austromontana         Phlox austromontana KANU00265432
2047         Phlox austromontana         Phlox austromontana KANU00265432
2048         Phlox austromontana         Phlox austromontana KANU00265433
2049                Phlox bifida                  Phlox bifida KSC0129901
2050                Phlox bifida                         Phlox bifida KSC
2051                Phlox bifida                         Phlox bifida KSC
2052                Phlox bifida                                     <NA>
2053                Phlox bifida                         Phlox bifida KSC
2054                Phlox bifida                  Phlox bifida KSC0129901
2055                Phlox bifida                  Phlox bifida KSC0129901
2056              Phlox buckleyi                                     <NA>
2057              Phlox buckleyi                Phlox buckleyi KSC0129905
2058              Phlox buckleyi                Phlox buckleyi KSC0129905
2059              Phlox buckleyi                Phlox buckleyi KSC0129904
2060              Phlox buckleyi                Phlox buckleyi KSC0129904
2061              Phlox buckleyi                Phlox buckleyi KSC0129905
2062              Phlox buckleyi                Phlox buckleyi KSC0129904
2063            Phlox caespitosa            Phlox caespitosa KANU00265455
2064            Phlox caespitosa                                     <NA>
2065            Phlox caespitosa            Phlox caespitosa KANU00265448
2066            Phlox caespitosa            Phlox caespitosa KANU00265455
2067            Phlox caespitosa            Phlox caespitosa KANU00265448
2068            Phlox caespitosa            Phlox caespitosa KANU00265448
2069            Phlox caespitosa            Phlox caespitosa KANU00265455
2070              Phlox carolina              Phlox carolina KANU00265453
2071              Phlox carolina              Phlox carolina KANU00265450
2072              Phlox carolina              Phlox carolina KANU00265453
2073              Phlox carolina              Phlox carolina KANU00265452
2074              Phlox carolina              Phlox carolina KANU00265450
2075              Phlox carolina              Phlox carolina KANU00265452
2076              Phlox carolina              Phlox carolina KANU00265452
2077              Phlox carolina                                     <NA>
2078              Phlox carolina              Phlox carolina KANU00265453
2079              Phlox carolina              Phlox carolina KANU00265450
2080              Phlox cluteana               Phlox cluteana RSABG780852
2081              Phlox cluteana                                     <NA>
2082              Phlox cluteana               Phlox cluteana RSABG780852
2083              Phlox cluteana               Phlox cluteana RSABG780852
2084             Phlox colubrina             Phlox colubrina KANU00265465
2085             Phlox colubrina             Phlox colubrina KANU00265465
2086             Phlox colubrina             Phlox colubrina KANU00265464
2087             Phlox colubrina                                     <NA>
2088             Phlox colubrina             Phlox colubrina KANU00265464
2089             Phlox colubrina             Phlox colubrina KANU00265464
2090            Phlox condensata            Phlox condensata KANU00330063
2091            Phlox condensata            Phlox condensata KANU00330063
2092            Phlox condensata                                     <NA>
2093            Phlox condensata            Phlox condensata KANU00330063
2094             Phlox cuspidata               Phlox cuspidata KSC0129916
2095             Phlox cuspidata               Phlox cuspidata KSC0129915
2096             Phlox cuspidata               Phlox cuspidata KSC0129917
2097             Phlox cuspidata               Phlox cuspidata KSC0129917
2098             Phlox cuspidata               Phlox cuspidata KSC0129915
2099             Phlox cuspidata             Phlox cuspidata KANU00265460
2100             Phlox cuspidata             Phlox cuspidata KANU00265458
2101             Phlox cuspidata               Phlox cuspidata KSC0129916
2102             Phlox cuspidata             Phlox cuspidata KANU00265458
2103             Phlox cuspidata               Phlox cuspidata KSC0129916
2104             Phlox cuspidata             Phlox cuspidata KANU00265458
2105             Phlox cuspidata               Phlox cuspidata KSC0129917
2106             Phlox cuspidata               Phlox cuspidata KSC0129915
2107             Phlox cuspidata             Phlox cuspidata KANU00265460
2108             Phlox cuspidata             Phlox cuspidata KANU00265460
2109             Phlox cuspidata                                     <NA>
2110               Phlox diffusa               Phlox diffusa KANU00265473
2111               Phlox diffusa               Phlox diffusa KANU00265469
2112               Phlox diffusa               Phlox diffusa KANU00265469
2113               Phlox diffusa               Phlox diffusa KANU00265473
2114               Phlox diffusa               Phlox diffusa KANU00265472
2115               Phlox diffusa                                     <NA>
2116               Phlox diffusa               Phlox diffusa KANU00265473
2117               Phlox diffusa               Phlox diffusa KANU00265472
2118               Phlox diffusa               Phlox diffusa KANU00265472
2119               Phlox diffusa               Phlox diffusa KANU00265469
2120              Phlox dispersa                 Phlox dispersa MO4331227
2121              Phlox dispersa                                     <NA>
2122              Phlox dispersa                 Phlox dispersa MO4331226
2123              Phlox dispersa                 Phlox dispersa MO4331226
2124              Phlox dispersa                 Phlox dispersa MO4331226
2125            Phlox divaricata              Phlox divaricata KANU348246
2126            Phlox divaricata              Phlox divaricata KSC0129933
2127            Phlox divaricata              Phlox divaricata KSC0129933
2128            Phlox divaricata              Phlox divaricata KANU348246
2129            Phlox divaricata              Phlox divaricata KANU348246
2130            Phlox divaricata              Phlox divaricata KANU349429
2131            Phlox divaricata              Phlox divaricata KANU349429
2132            Phlox divaricata              Phlox divaricata KSC0129959
2133            Phlox divaricata              Phlox divaricata KSC0129934
2134            Phlox divaricata              Phlox divaricata KSC0129934
2135            Phlox divaricata              Phlox divaricata KSC0129934
2136            Phlox divaricata              Phlox divaricata KSC0129933
2137            Phlox divaricata              Phlox divaricata KANU349429
2138            Phlox divaricata              Phlox divaricata KANU349877
2139            Phlox divaricata              Phlox divaricata KANU349877
2140            Phlox divaricata              Phlox divaricata KSC0129959
2141            Phlox divaricata              Phlox divaricata KANU349877
2142            Phlox divaricata              Phlox divaricata KSC0129959
2143            Phlox divaricata                    Phlox divaricata KSC2
2144            Phlox divaricata                     Phlox divaricata KSC
2145            Phlox divaricata                                     <NA>
2146            Phlox divaricata                     Phlox divaricata KSC
2147            Phlox divaricata                     Phlox divaricata KSC
2148            Phlox divaricata                    Phlox divaricata KSC2
2149           Phlox dolichantha                                     <NA>
2150           Phlox dolichantha             Phlox dolichantha KSC0129993
2151           Phlox dolichantha             Phlox dolichantha KSC0129993
2152           Phlox dolichantha              Phlox dolichantha KSC012992
2153           Phlox dolichantha              Phlox dolichantha KSC012992
2154           Phlox dolichantha             Phlox dolichantha KSC0129993
2155           Phlox dolichantha              Phlox dolichantha KSC012992
2156             Phlox douglasii             Phlox douglasii KANU00265555
2157             Phlox douglasii             Phlox douglasii KANU00265567
2158             Phlox douglasii             Phlox douglasii KANU00265567
2159             Phlox douglasii                                     <NA>
2160             Phlox douglasii             Phlox douglasii KANU00265555
2161             Phlox douglasii             Phlox douglasii KANU00265555
2162             Phlox douglasii             Phlox douglasii KANU00265567
2163            Phlox drummondii            Phlox drummondii KANU00265559
2164            Phlox drummondii            Phlox drummondii KANU00265559
2165            Phlox drummondii            Phlox drummondii KANU00265563
2166            Phlox drummondii                                     <NA>
2167            Phlox drummondii            Phlox drummondii KANU00265566
2168            Phlox drummondii            Phlox drummondii KANU00265559
2169            Phlox drummondii              Phlox drummondii KSC0130047
2170            Phlox drummondii              Phlox drummondii KSC0130051
2171            Phlox drummondii            Phlox drummondii KANU00265566
2172            Phlox drummondii              Phlox drummondii KSC0130047
2173            Phlox drummondii              Phlox drummondii KSC0130051
2174            Phlox drummondii            Phlox drummondii KANU00265563
2175            Phlox drummondii              Phlox drummondii KSC0130054
2176            Phlox drummondii              Phlox drummondii KSC0130051
2177            Phlox drummondii            Phlox drummondii KANU00265563
2178            Phlox drummondii              Phlox drummondii KSC0130047
2179            Phlox drummondii              Phlox drummondii KSC0130054
2180            Phlox drummondii            Phlox drummondii KANU00265566
2181            Phlox drummondii            Phlox drummondii KANU00265576
2182            Phlox drummondii            Phlox drummondii KANU00265576
2183            Phlox drummondii              Phlox drummondii KSC0130054
2184             Phlox floridana               Phlox floridana KSC0120060
2185             Phlox floridana               Phlox floridana KSC0130063
2186             Phlox floridana               Phlox floridana KSC0130061
2187             Phlox floridana               Phlox floridana KSC0130061
2188             Phlox floridana             Phlox floridana KANU00265594
2189             Phlox floridana               Phlox floridana KSC0130063
2190             Phlox floridana               Phlox floridana KSC0130063
2191             Phlox floridana                                     <NA>
2192             Phlox floridana             Phlox floridana KANU00265594
2193             Phlox floridana               Phlox floridana KSC0120060
2194             Phlox floridana             Phlox floridana KANU00265593
2195             Phlox floridana             Phlox floridana KANU00265594
2196             Phlox floridana             Phlox floridana KANU00265590
2197             Phlox floridana               Phlox floridana KSC0130061
2198             Phlox floridana               Phlox floridana KSC0120060
2199             Phlox floridana             Phlox floridana KANU00265593
2200             Phlox floridana             Phlox floridana KANU00265590
2201             Phlox floridana             Phlox floridana KANU00265590
2202            Phlox glaberrima              Phlox glaberrima KSC0130091
2203            Phlox glaberrima              Phlox glaberrima KSC0130094
2204            Phlox glaberrima            Phlox glaberrima KANU00265598
2205            Phlox glaberrima            Phlox glaberrima KANU00265598
2206            Phlox glaberrima              Phlox glaberrima KSC0130091
2207            Phlox glaberrima              Phlox glaberrima KSC0130091
2208            Phlox glaberrima              Phlox glaberrima KSC0130094
2209            Phlox glaberrima                                     <NA>
2210            Phlox glaberrima              Phlox glaberrima KSC0130094
2211            Phlox glaberrima            Phlox glaberrima KANU00265599
2212            Phlox glaberrima              Phlox glaberrima KSC0130086
2213            Phlox glaberrima            Phlox glaberrima KANU00265599
2214            Phlox glaberrima            Phlox glaberrima KANU00265598
2215            Phlox glaberrima              Phlox glaberrima KSC0130086
2216            Phlox glaberrima            Phlox glaberrima KANU00265599
2217            Phlox glaberrima              Phlox glaberrima KSC0130086
2218              Phlox griseola                                     <NA>
2219              Phlox griseola               Phlox griseola RSABG363552
2220              Phlox griseola               Phlox griseola RSABG363552
2221              Phlox griseola               Phlox griseola RSABG363552
2222                Phlox hoodii                Phlox hoodii KANU00265642
2223                Phlox hoodii                                     <NA>
2224                Phlox hoodii                Phlox hoodii KANU00265642
2225                Phlox hoodii                Phlox hoodii KANU00265642
2226            Phlox longifolia            Phlox longifolia KANU00265691
2227            Phlox longifolia            Phlox longifolia KANU00265696
2228            Phlox longifolia            Phlox longifolia KANU00265691
2229            Phlox longifolia                                     <NA>
2230            Phlox longifolia            Phlox longifolia KANU00265691
2231            Phlox longifolia            Phlox longifolia KANU00265696
2232            Phlox longifolia            Phlox longifolia KANU00265696
2233              Phlox maculata              Phlox maculata KANU00265733
2234              Phlox maculata              Phlox maculata KANU00265723
2235              Phlox maculata                Phlox maculata KSC0130160
2236              Phlox maculata              Phlox maculata KANU00265733
2237              Phlox maculata                Phlox maculata KSC0130160
2238              Phlox maculata              Phlox maculata KANU00265723
2239              Phlox maculata                Phlox maculata KSC0130160
2240              Phlox maculata              Phlox maculata KANU00265723
2241              Phlox maculata              Phlox maculata KANU00265733
2242              Phlox maculata                Phlox maculata KSC0130153
2243              Phlox maculata                Phlox maculata KSC0130156
2244              Phlox maculata                                     <NA>
2245              Phlox maculata                Phlox maculata KSC0130153
2246              Phlox maculata                Phlox maculata KSC0130156
2247              Phlox maculata                Phlox maculata KSC0130156
2248              Phlox maculata                Phlox maculata KSC0130153
2249            Phlox multiflora            Phlox multiflora KANU00322730
2250            Phlox multiflora            Phlox multiflora KANU00324002
2251            Phlox multiflora                                     <NA>
2252            Phlox multiflora            Phlox multiflora KANU00322730
2253            Phlox multiflora            Phlox multiflora KANU00322730
2254            Phlox multiflora            Phlox multiflora KANU00265737
2255            Phlox multiflora            Phlox multiflora KANU00324002
2256            Phlox multiflora            Phlox multiflora KANU00324002
2257            Phlox multiflora            Phlox multiflora KANU00265737
2258            Phlox multiflora            Phlox multiflora KANU00265737
2259             Phlox muscoides             Phlox muscoides KANU00265750
2260             Phlox muscoides             Phlox muscoides KANU00265750
2261             Phlox muscoides             Phlox muscoides KANU00265757
2262             Phlox muscoides             Phlox muscoides KANU00265750
2263             Phlox muscoides             Phlox muscoides KANU00265757
2264             Phlox muscoides             Phlox muscoides KANU00307760
2265             Phlox muscoides             Phlox muscoides KANU00307760
2266             Phlox muscoides             Phlox muscoides KANU00265757
2267             Phlox muscoides             Phlox muscoides KANU00307760
2268             Phlox muscoides                                     <NA>
2269                  Phlox nana                  Phlox nana KANU00265768
2270                  Phlox nana                  Phlox nana KANU00265772
2271                  Phlox nana                  Phlox nana KANU00265770
2272                  Phlox nana                  Phlox nana KANU00265768
2273                  Phlox nana                  Phlox nana KANU00265770
2274                  Phlox nana                  Phlox nana KANU00265770
2275                  Phlox nana                  Phlox nana KANU00265772
2276                  Phlox nana                  Phlox nana KANU00265772
2277                  Phlox nana                  Phlox nana KANU00265768
2278                  Phlox nana                                     <NA>
2279               Phlox nivalis               Phlox nivalis KANU00265779
2280               Phlox nivalis               Phlox nivalis KANU00265779
2281               Phlox nivalis               Phlox nivalis KANU00265779
2282               Phlox nivalis                                     <NA>
2283               Phlox nivalis                 Phlox nivalis KSC0130173
2284               Phlox nivalis               Phlox nivalis KANU00265780
2285               Phlox nivalis               Phlox nivalis KANU00265780
2286               Phlox nivalis               Phlox nivalis KANU00265780
2287               Phlox nivalis                 Phlox nivalis KSC0130173
2288               Phlox nivalis                 Phlox nivalis KSC0130173
2289               Phlox nivalis               Phlox nivalis KANU00265781
2290               Phlox nivalis               Phlox nivalis KANU00265781
2291               Phlox nivalis               Phlox nivalis KANU00265781
2292          Phlox oklahomensis          Phlox oklahomensis KANU00330629
2293          Phlox oklahomensis          Phlox oklahomensis KANU00330629
2294          Phlox oklahomensis          Phlox oklahomensis KANU00330629
2295          Phlox oklahomensis                                     <NA>
2296          Phlox oklahomensis            Phlox oklahomensis KSC0130184
2297          Phlox oklahomensis            Phlox oklahomensis KSC0130184
2298          Phlox oklahomensis            Phlox oklahomensis KANU350240
2299          Phlox oklahomensis            Phlox oklahomensis KANU350240
2300          Phlox oklahomensis            Phlox oklahomensis KSC0130181
2301          Phlox oklahomensis          Phlox oklahomensis KANU00043577
2302          Phlox oklahomensis            Phlox oklahomensis KSC0130184
2303          Phlox oklahomensis            Phlox oklahomensis KSC0130180
2304          Phlox oklahomensis            Phlox oklahomensis KSC0130181
2305          Phlox oklahomensis            Phlox oklahomensis KANU350240
2306          Phlox oklahomensis          Phlox oklahomensis KANU00043577
2307          Phlox oklahomensis            Phlox oklahomensis KSC0130181
2308          Phlox oklahomensis            Phlox oklahomensis KSC0130180
2309          Phlox oklahomensis          Phlox oklahomensis KANU00043577
2310          Phlox oklahomensis            Phlox oklahomensis KSC0130180
2311             Phlox opalensis               Phlox opalensis MO04860164
2312             Phlox opalensis               Phlox opalensis MO04860164
2313             Phlox opalensis               Phlox opalensis MO04860164
2314             Phlox opalensis                                     <NA>
2315                 Phlox ovata                 Phlox ovata KANU00265785
2316                 Phlox ovata                 Phlox ovata KANU00265786
2317                 Phlox ovata                 Phlox ovata KANU00265786
2318                 Phlox ovata                 Phlox ovata KANU00265785
2319                 Phlox ovata                 Phlox ovata KANU00265786
2320                 Phlox ovata                 Phlox ovata KANU00265785
2321                 Phlox ovata                                     <NA>
2322            Phlox paniculata            Phlox paniculata KANU00078424
2323            Phlox paniculata            Phlox paniculata KANU00078426
2324            Phlox paniculata            Phlox paniculata KANU00078426
2325            Phlox paniculata            Phlox paniculata KANU00078426
2326            Phlox paniculata              Phlox paniculata KANU353051
2327            Phlox paniculata              Phlox paniculata KANU353051
2328            Phlox paniculata              Phlox paniculata KSC0130191
2329            Phlox paniculata              Phlox paniculata KSC0130191
2330            Phlox paniculata              Phlox paniculata KSC0130191
2331            Phlox paniculata            Phlox paniculata KANU00078424
2332            Phlox paniculata              Phlox paniculata KSC0130189
2333            Phlox paniculata              Phlox paniculata KSC0130188
2334            Phlox paniculata              Phlox paniculata KSC0130188
2335            Phlox paniculata                                     <NA>
2336            Phlox paniculata            Phlox paniculata KANU00078424
2337            Phlox paniculata              Phlox paniculata KSC0130189
2338            Phlox paniculata              Phlox paniculata KANU353051
2339            Phlox paniculata              Phlox paniculata KSC0130188
2340            Phlox paniculata              Phlox paniculata KSC0130189
2341           Phlox pattersonii                   Phlox pattersonii KSC2
2342           Phlox pattersonii                   Phlox pattersonii KSC2
2343           Phlox pattersonii                                     <NA>
2344                Phlox pilosa                  Phlox pilosa KSC0130223
2345                Phlox pilosa                  Phlox pilosa KSC0130228
2346                Phlox pilosa                  Phlox pilosa KSC0130223
2347                Phlox pilosa                  Phlox pilosa KSC0130205
2348                Phlox pilosa                  Phlox pilosa KSC0130205
2349                Phlox pilosa                  Phlox pilosa KSC0130228
2350                Phlox pilosa                  Phlox pilosa KSC0130223
2351                Phlox pilosa                  Phlox pilosa KSC0130228
2352                Phlox pilosa                                     <NA>
2353                Phlox pilosa                  Phlox pilosa KSC0130205
2354           Phlox pulcherrima                Phlox pulcherrima MO13544
2355           Phlox pulcherrima               Phlox pulcherrima MO135546
2356           Phlox pulcherrima                Phlox pulcherrima MO13544
2357           Phlox pulcherrima               Phlox pulcherrima MO135546
2358           Phlox pulcherrima                Phlox pulcherrima MO13544
2359           Phlox pulcherrima                Phlox pulcherrima MO90652
2360           Phlox pulcherrima                Phlox pulcherrima MO90652
2361           Phlox pulcherrima                                     <NA>
2362           Phlox pulcherrima               Phlox pulcherrima MO135546
2363             Phlox pulvinata             Phlox pulvinata KANU00310029
2364             Phlox pulvinata             Phlox pulvinata KANU00310029
2365             Phlox pulvinata             Phlox pulvinata KANU00265943
2366             Phlox pulvinata             Phlox pulvinata KANU00318200
2367             Phlox pulvinata             Phlox pulvinata KANU00265943
2368             Phlox pulvinata             Phlox pulvinata KANU00318200
2369             Phlox pulvinata             Phlox pulvinata KANU00310029
2370             Phlox pulvinata             Phlox pulvinata KANU00318200
2371             Phlox pulvinata             Phlox pulvinata KANU00265943
2372             Phlox pulvinata                                     <NA>
2373               Phlox pungens                                     <NA>
2374               Phlox pungens                  Phlox pungens MO3904631
2375               Phlox pungens                  Phlox pungens MO3904632
2376               Phlox pungens                  Phlox pungens MO3904631
2377               Phlox pungens                  Phlox pungens MO3904632
2378               Phlox pungens                  Phlox pungens MO3904632
2379               Phlox pungens                  Phlox pungens MO3904631
2380            Phlox roemeriana            Phlox roemeriana KANU00265947
2381            Phlox roemeriana            Phlox roemeriana KANU00265950
2382            Phlox roemeriana            Phlox roemeriana KANU00265950
2383            Phlox roemeriana            Phlox roemeriana KANU00265946
2384            Phlox roemeriana            Phlox roemeriana KANU00265946
2385            Phlox roemeriana            Phlox roemeriana KANU00265947
2386            Phlox roemeriana            Phlox roemeriana KANU00265947
2387            Phlox roemeriana            Phlox roemeriana KANU00265950
2388            Phlox roemeriana                                     <NA>
2389            Phlox roemeriana            Phlox roemeriana KANU00265946
2390            Phlox stansburyi            Phlox stansburyi KANU00265960
2391            Phlox stansburyi            Phlox stansburyi KANU00265960
2392            Phlox stansburyi            Phlox stansburyi KANU00265960
2393            Phlox stansburyi                                     <NA>
2394            Phlox stansburyi            Phlox stansburyi KANU00265962
2395            Phlox stansburyi            Phlox stansburyi KANU00265962
2396            Phlox stansburyi            Phlox stansburyi KANU00265962
2397           Phlox stolonifera           Phlox stolonifera KANU00265964
2398           Phlox stolonifera             Phlox stolonifera KSC0130332
2399           Phlox stolonifera           Phlox stolonifera KANU00324226
2400           Phlox stolonifera           Phlox stolonifera KANU00324226
2401           Phlox stolonifera                                     <NA>
2402           Phlox stolonifera           Phlox stolonifera KANU00265964
2403           Phlox stolonifera             Phlox stolonifera KSC0130330
2404           Phlox stolonifera             Phlox stolonifera KSC0130330
2405           Phlox stolonifera           Phlox stolonifera KANU00265964
2406           Phlox stolonifera             Phlox stolonifera KSC0130332
2407           Phlox stolonifera             Phlox stolonifera KSC0130332
2408           Phlox stolonifera             Phlox stolonifera KSC0130330
2409           Phlox stolonifera           Phlox stolonifera KANU00324226
2410           Phlox stolonifera             Phlox stolonifera KSC0130331
2411              Phlox subulata              Phlox subulata KANU00265968
2412              Phlox subulata              Phlox subulata KANU00265966
2413              Phlox subulata                Phlox subulata KSC0130334
2414              Phlox subulata                Phlox subulata KSC0130334
2415              Phlox subulata              Phlox subulata KANU00265966
2416              Phlox subulata              Phlox subulata KANU00265973
2417              Phlox subulata              Phlox subulata KANU00265973
2418              Phlox subulata              Phlox subulata KANU00265968
2419              Phlox subulata              Phlox subulata KANU00265968
2420              Phlox subulata                Phlox subulata KSC0130333
2421              Phlox subulata                Phlox subulata KSC0130336
2422              Phlox subulata                                     <NA>
2423              Phlox subulata                Phlox subulata KSC0130336
2424              Phlox subulata                Phlox subulata KSC0130336
2425              Phlox subulata              Phlox subulata KANU00265974
2426              Phlox subulata              Phlox subulata KANU00265974
2427              Phlox subulata              Phlox subulata KANU00265973
2428              Phlox subulata              Phlox subulata KANU00265974
2429              Phlox subulata              Phlox subulata KANU00265966
2430              Phlox subulata                Phlox subulata KSC0130333
2431              Phlox subulata                Phlox subulata KSC0130333
2432            Phlox tenuifolia            Phlox tenuifolia KANU00265972
2433            Phlox tenuifolia            Phlox tenuifolia KANU00265972
2434            Phlox tenuifolia            Phlox tenuifolia KANU00265972
2435            Phlox tenuifolia                                     <NA>
2436               Phlox viscida               Phlox viscida KANU00265970
2437               Phlox viscida               Phlox viscida KANU00265970
2438               Phlox viscida               Phlox viscida KANU00265971
2439               Phlox viscida               Phlox viscida KANU00265971
2440               Phlox viscida                                     <NA>
2441               Phlox viscida               Phlox viscida KANU00265971
2442               Phlox viscida               Phlox viscida KANU00265970
2443            Phlox woodhousei              Phlox woodhousei NYBG505447
2444            Phlox woodhousei              Phlox woodhousei NYBG505447
2445            Phlox woodhousei              Phlox woodhousei NYBG505447
2446            Phlox woodhousei              Phlox woodhousei NYBG392455
2447            Phlox woodhousei              Phlox woodhousei NYBG392455
2448            Phlox woodhousei              Phlox woodhousei NYBG405562
2449            Phlox woodhousei              Phlox woodhousei NYBG384641
2450            Phlox woodhousei              Phlox woodhousei NYBG405562
2451            Phlox woodhousei              Phlox woodhousei NYBG405562
2452            Phlox woodhousei                                     <NA>
2453            Phlox woodhousei              Phlox woodhousei NYBG393318
2454            Phlox woodhousei              Phlox woodhousei NYBG384641
2455            Phlox woodhousei              Phlox woodhousei NYBG384641
2456            Phlox woodhousei              Phlox woodhousei NYBG392455
2457            Phlox woodhousei            Phlox woodhousei KANU00265969
2458            Phlox woodhousei              Phlox woodhousei NYBG393318
2459            Phlox woodhousei              Phlox woodhousei NYBG393318
2460            Phlox woodhousei            Phlox woodhousei KANU00265969
2461            Phlox woodhousei            Phlox woodhousei KANU00265969
2462      Polemonium acutiflorum      Polemonium acutiflorum KANU00331057
2463      Polemonium acutiflorum                                     <NA>
2464      Polemonium acutiflorum      Polemonium acutiflorum KANU00331057
2465      Polemonium acutiflorum      Polemonium acutiflorum KANU00331058
2466      Polemonium acutiflorum      Polemonium acutiflorum KANU00263978
2467      Polemonium acutiflorum      Polemonium acutiflorum KANU00331058
2468      Polemonium acutiflorum      Polemonium acutiflorum KANU00331058
2469      Polemonium acutiflorum      Polemonium acutiflorum KANU00331057
2470      Polemonium acutiflorum      Polemonium acutiflorum KANU00263978
2471      Polemonium acutiflorum      Polemonium acutiflorum KANU00263978
2472          Polemonium boreale          Polemonium boreale KANU00263983
2473          Polemonium boreale                                     <NA>
2474          Polemonium boreale          Polemonium boreale KANU00263983
2475          Polemonium boreale          Polemonium boreale KANU00263983
2476        Polemonium caeruleum        Polemonium caeruleum KANU00263984
2477        Polemonium caeruleum                                     <NA>
2478        Polemonium caeruleum        Polemonium caeruleum KANU00263984
2479     Polemonium californicum     Polemonium californicum KANU00263987
2480     Polemonium californicum     Polemonium californicum KANU00263987
2481     Polemonium californicum     Polemonium californicum KANU00263988
2482     Polemonium californicum     Polemonium californicum KANU00263987
2483     Polemonium californicum     Polemonium californicum KANU00263988
2484     Polemonium californicum       Polemonium californicum KSC0130367
2485     Polemonium californicum       Polemonium californicum KSC0130367
2486     Polemonium californicum     Polemonium californicum KANU00263988
2487     Polemonium californicum       Polemonium californicum KSC0130367
2488     Polemonium californicum                                     <NA>
2489          Polemonium carneum            Polemonium carneum KSC0130368
2490          Polemonium carneum            Polemonium carneum KSC0130368
2491          Polemonium carneum                                     <NA>
2492          Polemonium carneum            Polemonium carneum KSC0130368
2493       Polemonium caucasicum               Polemonium caucasicum NYBG
2494       Polemonium caucasicum               Polemonium caucasicum NYBG
2495       Polemonium caucasicum               Polemonium caucasicum NYBG
2496       Polemonium caucasicum             Polemonium caucasicum NYBG 2
2497       Polemonium caucasicum             Polemonium caucasicum NYBG 4
2498       Polemonium caucasicum             Polemonium caucasicum NYBG 4
2499       Polemonium caucasicum             Polemonium caucasicum NYBG 2
2500       Polemonium caucasicum             Polemonium caucasicum NYBG 4
2501       Polemonium caucasicum                                     <NA>
2502       Polemonium chartaceum           Polemonium chartaceum MO997483
2503       Polemonium chartaceum           Polemonium chartaceum MO997483
2504       Polemonium chartaceum           Polemonium chartaceum MO997483
2505       Polemonium chartaceum                                     <NA>
2506        Polemonium confertum           Polemonium confertum RMH776743
2507        Polemonium confertum                                     <NA>
2508        Polemonium confertum        Polemonium confertum KANU00263989
2509        Polemonium confertum        Polemonium confertum KANU00263989
2510        Polemonium confertum          Polemonium confertum KSC0130369
2511        Polemonium confertum          Polemonium confertum KSC0130369
2512        Polemonium confertum          Polemonium confertum KSC0130369
2513        Polemonium confertum           Polemonium confertum RMH736397
2514        Polemonium confertum           Polemonium confertum RMH776743
2515        Polemonium confertum           Polemonium confertum RMH776743
2516        Polemonium confertum           Polemonium confertum RMH721962
2517        Polemonium confertum           Polemonium confertum RMH721974
2518        Polemonium confertum           Polemonium confertum RMH721962
2519        Polemonium confertum           Polemonium confertum RMH721962
2520        Polemonium confertum           Polemonium confertum RMH736397
2521        Polemonium confertum           Polemonium confertum RMH736397
2522        Polemonium delicatum        Polemonium delicatum KANU00263992
2523        Polemonium delicatum        Polemonium delicatum KANU00263991
2524        Polemonium delicatum        Polemonium delicatum KANU00263993
2525        Polemonium delicatum        Polemonium delicatum KANU00263992
2526        Polemonium delicatum        Polemonium delicatum KANU00263991
2527        Polemonium delicatum        Polemonium delicatum KANU00263991
2528        Polemonium delicatum        Polemonium delicatum KANU00263993
2529        Polemonium delicatum        Polemonium delicatum KANU00263993
2530        Polemonium delicatum        Polemonium delicatum KANU00263992
2531        Polemonium delicatum                                     <NA>
2532          Polemonium elegans             Polemonium elegans MO1012176
2533          Polemonium elegans             Polemonium elegans MO1012176
2534          Polemonium elegans              Polemonium elegans MO991425
2535          Polemonium elegans             Polemonium elegans MO2751520
2536          Polemonium elegans             Polemonium elegans MO2751520
2537          Polemonium elegans              Polemonium elegans MO991425
2538          Polemonium elegans              Polemonium elegans MO991425
2539          Polemonium elegans             Polemonium elegans MO2751520
2540          Polemonium elegans             Polemonium elegans MO1012176
2541          Polemonium elegans                                     <NA>
2542          Polemonium eximium                                     <NA>
2543          Polemonium eximium             Polemonium eximium MO2751795
2544          Polemonium eximium             Polemonium eximium MO2751795
2545          Polemonium eximium             Polemonium eximium MO2751797
2546    Polemonium foliosissimum    Polemonium foliosissimum KANU00268633
2547    Polemonium foliosissimum      Polemonium foliosissimum KSC0130375
2548    Polemonium foliosissimum      Polemonium foliosissimum KSC0130375
2549    Polemonium foliosissimum      Polemonium foliosissimum KSC0130374
2550    Polemonium foliosissimum      Polemonium foliosissimum KSC0130376
2551    Polemonium foliosissimum      Polemonium foliosissimum KSC0130376
2552    Polemonium foliosissimum                                     <NA>
2553    Polemonium foliosissimum      Polemonium foliosissimum KSC0130375
2554    Polemonium foliosissimum      Polemonium foliosissimum KSC0130374
2555    Polemonium foliosissimum      Polemonium foliosissimum KSC0130374
2556    Polemonium foliosissimum    Polemonium foliosissimum KANU00268634
2557    Polemonium foliosissimum    Polemonium foliosissimum KANU00268633
2558    Polemonium foliosissimum    Polemonium foliosissimum KANU00268633
2559    Polemonium foliosissimum    Polemonium foliosissimum KANU00268634
2560    Polemonium foliosissimum    Polemonium foliosissimum KANU00335551
2561    Polemonium foliosissimum    Polemonium foliosissimum KANU00268634
2562    Polemonium foliosissimum    Polemonium foliosissimum KANU00335551
2563    Polemonium foliosissimum    Polemonium foliosissimum KANU00335551
2564    Polemonium foliosissimum      Polemonium foliosissimum KSC0130371
2565    Polemonium foliosissimum      Polemonium foliosissimum KSC0130371
2566    Polemonium foliosissimum      Polemonium foliosissimum KSC0130371
2567     Polemonium grandiflorum                                     <NA>
2568     Polemonium grandiflorum        Polemonium grandiflorum MO2751529
2569     Polemonium grandiflorum        Polemonium grandiflorum MO2751529
2570     Polemonium grandiflorum        Polemonium grandiflorum MO2751530
2571     Polemonium grandiflorum        Polemonium grandiflorum MO2751529
2572     Polemonium grandiflorum        Polemonium grandiflorum MO2751731
2573     Polemonium grandiflorum        Polemonium grandiflorum MO2751530
2574        Polemonium mexicanum           Polemonium mexicanum MO1736468
2575        Polemonium mexicanum           Polemonium mexicanum MO1129129
2576        Polemonium mexicanum           Polemonium mexicanum MO1129129
2577        Polemonium mexicanum                                     <NA>
2578        Polemonium mexicanum           Polemonium mexicanum MO1736468
2579        Polemonium mexicanum           Polemonium mexicanum MO1129129
2580        Polemonium mexicanum           Polemonium mexicanum MO1736468
2581        Polemonium mexicanum           Polemonium mexicanum MO2751532
2582        Polemonium mexicanum           Polemonium mexicanum MO2751532
2583        Polemonium mexicanum           Polemonium mexicanum MO2751532
2584       Polemonium micranthum       Polemonium micranthum KANU00343808
2585       Polemonium micranthum       Polemonium micranthum KANU00314510
2586       Polemonium micranthum         Polemonium micranthum KSC0130380
2587       Polemonium micranthum         Polemonium micranthum KSC0130380
2588       Polemonium micranthum         Polemonium micranthum KSC0130379
2589       Polemonium micranthum         Polemonium micranthum KSC0130379
2590       Polemonium micranthum         Polemonium micranthum KSC0130380
2591       Polemonium micranthum                                     <NA>
2592      Polemonium occidentale        Polemonium occidentale KSC0130382
2593      Polemonium occidentale        Polemonium occidentale KSC0130383
2594      Polemonium occidentale      Polemonium occidentale KANU00319427
2595      Polemonium occidentale        Polemonium occidentale KANU363104
2596      Polemonium occidentale        Polemonium occidentale KANU363104
2597      Polemonium occidentale        Polemonium occidentale KSC0130385
2598      Polemonium occidentale        Polemonium occidentale KSC0130385
2599      Polemonium occidentale        Polemonium occidentale KSC0130387
2600      Polemonium occidentale        Polemonium occidentale KSC0130387
2601      Polemonium occidentale        Polemonium occidentale KSC0130387
2602      Polemonium occidentale                                     <NA>
2603      Polemonium occidentale      Polemonium occidentale KANU00318757
2604      Polemonium occidentale      Polemonium occidentale KANU00318757
2605      Polemonium occidentale      Polemonium occidentale KANU00318757
2606      Polemonium occidentale      Polemonium occidentale KANU00319427
2607      Polemonium occidentale      Polemonium occidentale KANU00319427
2608      Polemonium pauciflorum         Polemonium pauciflorum MO1229549
2609      Polemonium pauciflorum         Polemonium pauciflorum MO2751940
2610      Polemonium pauciflorum                                     <NA>
2611       Polemonium pectinatum          Polemonium pectinatum MO2751942
2612       Polemonium pectinatum                                     <NA>
2613       Polemonium pulchellum       Polemonium pulchellum KANU00331059
2614       Polemonium pulchellum       Polemonium pulchellum KANU00331059
2615       Polemonium pulchellum       Polemonium pulchellum KANU00331060
2616       Polemonium pulchellum       Polemonium pulchellum KANU00331059
2617       Polemonium pulchellum       Polemonium pulchellum KANU00331060
2618       Polemonium pulchellum                                     <NA>
2619       Polemonium pulchellum       Polemonium pulchellum KANU00331060
2620     Polemonium pulcherrimum     Polemonium pulcherrimum KANU00331042
2621     Polemonium pulcherrimum       Polemonium pulcherrimum KSC0130389
2622     Polemonium pulcherrimum                                     <NA>
2623     Polemonium pulcherrimum     Polemonium pulcherrimum KANU00331042
2624     Polemonium pulcherrimum       Polemonium pulcherrimum KSC0130388
2625     Polemonium pulcherrimum       Polemonium pulcherrimum KSC0130389
2626     Polemonium pulcherrimum     Polemonium pulcherrimum KANU00331042
2627          Polemonium reptans          Polemonium reptans KANU00080962
2628          Polemonium reptans          Polemonium reptans KANU00080962
2629          Polemonium reptans          Polemonium reptans KANU00311734
2630          Polemonium reptans          Polemonium reptans KANU00311734
2631          Polemonium reptans          Polemonium reptans KANU00311734
2632          Polemonium reptans            Polemonium reptans KSC0130401
2633          Polemonium reptans            Polemonium reptans KSC0130401
2634          Polemonium reptans            Polemonium reptans KSC0130404
2635          Polemonium reptans            Polemonium reptans KSC0130404
2636          Polemonium reptans            Polemonium reptans KSC0130404
2637          Polemonium reptans            Polemonium reptans KSC0130405
2638          Polemonium reptans            Polemonium reptans KSC0130405
2639          Polemonium reptans            Polemonium reptans KSC0130405
2640          Polemonium reptans                                     <NA>
2641          Polemonium reptans            Polemonium reptans KSC0130403
2642          Polemonium reptans            Polemonium reptans KSC0130403
2643          Polemonium reptans            Polemonium reptans KSC0130401
2644          Polemonium reptans            Polemonium reptans KSC0130403
2645          Polemonium reptans          Polemonium reptans KANU00080960
2646          Polemonium reptans          Polemonium reptans KANU00080960
2647          Polemonium reptans          Polemonium reptans KANU00080960
2648         Polemonium viscosum         Polemonium viscosum KANU00268697
2649         Polemonium viscosum         Polemonium viscosum KANU00268697
2650         Polemonium viscosum           Polemonium viscosum KSC0130410
2651         Polemonium viscosum           Polemonium viscosum KSC0130410
2652         Polemonium viscosum           Polemonium viscosum KSC0130410
2653         Polemonium viscosum                                     <NA>
2654         Polemonium viscosum         Polemonium viscosum KANU00268698
2655         Polemonium viscosum         Polemonium viscosum KANU00268698
2656         Polemonium viscosum         Polemonium viscosum KANU00268698
2657         Polemonium viscosum         Polemonium viscosum KANU00268697
2658         Polemonium viscosum         Polemonium viscosum KANU00268704
2659         Polemonium viscosum         Polemonium viscosum KANU00268704
2660        Saltugilia australis                                     <NA>
2661        Saltugilia australis           Saltugilia australis MO2684686
2662        Saltugilia australis         Saltugilia australis RSABG672125
2663        Saltugilia australis         Saltugilia australis RSABG599790
2664        Saltugilia australis         Saltugilia australis RSABG599790
2665        Saltugilia australis         Saltugilia australis RSABG672125
2666        Saltugilia australis         Saltugilia australis RSABG672125
2667        Saltugilia australis           Saltugilia australis MO2684686
2668        Saltugilia australis           Saltugilia australis MO2684686
2669        Saltugilia australis           Saltugilia australis MO4000368
2670        Saltugilia australis           Saltugilia australis MO3890157
2671        Saltugilia australis           Saltugilia australis MO3890157
2672        Saltugilia australis           Saltugilia australis MO3890157
2673        Saltugilia australis           Saltugilia australis MO4000368
2674        Saltugilia australis           Saltugilia australis MO4000368
2675        Saltugilia australis         Saltugilia australis RSABG599790
2676       Saltugilia caruifolia       Saltugilia caruifolia KANU00237633
2677       Saltugilia caruifolia        Saltugilia caruifolia RSABG731741
2678       Saltugilia caruifolia                                     <NA>
2679       Saltugilia caruifolia       Saltugilia caruifolia KANU00237633
2680       Saltugilia caruifolia       Saltugilia caruifolia KANU00237633
2681       Saltugilia caruifolia        Saltugilia caruifolia RSABG519433
2682       Saltugilia caruifolia        Saltugilia caruifolia RSABG519433
2683       Saltugilia caruifolia        Saltugilia caruifolia RSABG519433
2684       Saltugilia caruifolia        Saltugilia caruifolia RSABG731741
2685       Saltugilia caruifolia        Saltugilia caruifolia RSABG731741
2686         Saltugilia latimeri                                     <NA>
2687         Saltugilia latimeri          Saltugilia latimeri RSABG720349
2688         Saltugilia latimeri          Saltugilia latimeri RSABG720349
2689         Saltugilia latimeri          Saltugilia latimeri RSABG720349
     flower_number corolla_length_cm corolla_width_throat_cm     color_opt_1
1                1             3.696                   0.455           white
2             <NA>              <NA>                    <NA>           white
3                3             2.874                   0.375           white
4                1             3.552                   0.393           white
5                2              2.66                    0.37           white
6                1             1.652                   0.241             red
7                2             1.664                   0.148             red
8             <NA>              <NA>                    <NA>             red
9                3             1.885                   0.201             red
10            <NA>              <NA>                    <NA>            pink
11               1             2.216                   0.309            pink
12               2             2.124                   0.286            pink
13               1             2.088                   0.386            pink
14               3             2.052                    0.23            pink
15               3             2.161                    0.29            pink
16               1              1.97                   0.201            pink
17               2              2.03                   0.248            pink
18               2             2.172                    0.35            pink
19               3             2.239                   0.375            pink
20               2             1.109                   0.221            pink
21               1             1.061                   0.124            pink
22               2             1.251                    0.13            pink
23               3             1.148                   0.188            pink
24               1             2.195                   0.233            pink
25               1             1.715                   0.159            pink
26            <NA>              <NA>                    <NA>            pink
27               2             1.905                   0.192            pink
28               3             1.721                   0.253            pink
29               2             2.153                   0.319            pink
30               1             1.673                   0.238            pink
31               3             2.214                   0.292            pink
32               3             1.563                   0.239            pink
33               3             1.201                   0.123            pink
34               2             1.604                   0.321            pink
35               1             1.202                    0.11            pink
36               2             1.452                   0.124            pink
37               1             0.761                   0.184            pink
38               3             0.949                   0.141            pink
39            <NA>              <NA>                    <NA>            pink
40               1             1.115                   0.131            pink
41               2             0.905                    0.12            pink
42               3             1.467                   0.168            pink
43               1             0.881                   0.172           white
44            <NA>              <NA>                    <NA>           white
45               1             0.739                   0.144           white
46               3             0.957                   0.272           white
47               3             0.609                   0.085           white
48               2             0.916                   0.161           white
49               2             0.803                   0.116           white
50               1              0.56                   0.086            pink
51               2             0.559                   0.057            pink
52               3             0.529                   0.073            pink
53               2             0.544                    0.11            pink
54               3             0.544                   0.103            pink
55               2               0.6                   0.104            pink
56               1             0.561                     0.1            pink
57               3             0.539                    0.12            pink
58            <NA>              <NA>                    <NA>            pink
59               1             0.568                   0.114            pink
60               3             0.395                   0.074           white
61               3             0.519                   0.088           white
62               2             0.635                   0.115           white
63               2              0.45                   0.084           white
64               1             0.731                   0.141           white
65               1             0.508                   0.083           white
66               3             0.551                   0.114           white
67               1             0.395                   0.084           white
68               2             0.478                   0.084           white
69            <NA>              <NA>                    <NA>           white
70            <NA>              <NA>                    <NA>           white
71               3             0.653                   0.128           white
72               2             0.616                   0.103           white
73               1             0.639                   0.154           white
74               1             0.668                   0.128            blue
75               3             0.616                   0.152            blue
76            <NA>              <NA>                    <NA>            blue
77               2              0.71                   0.144            blue
78               2             0.213                   0.049           white
79               1             0.202                   0.038           white
80            <NA>              <NA>                    <NA>           white
81               3             0.224                   0.041           white
82               3             0.424                    0.16           white
83               2             0.663                   0.089           white
84               1             0.567                   0.117           white
85               2             0.411                   0.139           white
86               3             0.506                   0.102           white
87               2             0.759                    0.11           white
88               2             0.423                   0.144           white
89            <NA>              <NA>                    <NA>           white
90               1             0.522                   0.145           white
91               3             0.497                   0.056           white
92               3               0.5                   0.112           white
93               1             0.501                    0.11           white
94               1             0.516                   0.111           white
95               2             0.964                   0.239            pink
96               1             1.407                   0.235            pink
97               1             1.516                   0.298            pink
98               1             1.527                   0.212            pink
99               2             1.551                   0.197            pink
100              1              1.51                   0.147            pink
101              2             1.591                   0.162            pink
102              2             1.332                    0.27            pink
103              3             1.037                   0.208            pink
104              3             1.449                   0.187            pink
105              3             1.176                   0.273            pink
106           <NA>              <NA>                    <NA>            pink
107              3             1.223                    0.17            pink
108              3             0.565                    0.15            blue
109              3              <NA>                    <NA>            blue
110              1              0.57                   0.175            blue
111           <NA>              <NA>                    <NA>            blue
112              3             0.666                   0.196            blue
113              2             0.612                   0.142            blue
114              1              0.62                   0.149            blue
115              2              <NA>                    <NA>            blue
116              2             0.624                   0.163            blue
117              1              <NA>                    <NA>            blue
118              1             0.907                   0.291            pink
119              3              <NA>                    <NA>            pink
120              1              <NA>                    <NA>            pink
121           <NA>              <NA>                    <NA>            pink
122              2             0.925                   0.288            pink
123              3             0.898                   0.278            pink
124              2              <NA>                    <NA>            pink
125              2              0.73                   0.154           white
126              3             0.987                   0.227           white
127              2             0.806                   0.222           white
128              3              0.85                   0.209           white
129              1             1.044                    0.28           white
130              1               0.8                   0.189           white
131              2             1.068                   0.223           white
132              1             0.933                   0.211           white
133           <NA>              <NA>                    <NA>           white
134              3             1.015                   0.217           white
135              3             0.451                   0.102           white
136              2             0.579                   0.143           white
137              1             0.647                   0.122           white
138              2             0.363                   0.117           white
139           <NA>              <NA>                    <NA>           white
140              1             0.362                   0.106           white
141              3             0.717                   0.161           white
142              3             1.448                   0.225             red
143              2             1.503                   0.207             red
144              3             1.875                   0.277             red
145              1             1.792                   0.293             red
146              3             1.681                   0.241             red
147              2             1.355                   0.204             red
148              2             1.425                    0.23             red
149              2             1.541                   0.197             red
150              3             1.621                   0.246             red
151              2             1.781                   0.273             red
152           <NA>              <NA>                    <NA>             red
153              3             1.465                   0.254             red
154              1             1.424                    0.22             red
155              1             1.714                   0.221             red
156              2             1.328                   0.194             red
157              1             1.501                   0.194             red
158              1              1.31                   0.185             red
159              1             1.481                   0.248             red
160              3             1.592                   0.193             red
161              2             0.859                   0.142            blue
162              1             0.884                    0.13            blue
163           <NA>              <NA>                    <NA>            blue
164              3              0.94                   0.144            blue
165              1             0.423                    0.08           white
166              1             0.473                   0.075           white
167              2             0.429                   0.065           white
168              2             0.396                    0.06           white
169              3             0.404                   0.062           white
170              3             0.486                   0.061           white
171              2             0.428                   0.082           white
172              1             0.383                   0.073           white
173           <NA>              <NA>                    <NA>           white
174              2             0.633                   0.106            pink
175           <NA>              <NA>                    <NA>            pink
176              3             0.632                   0.111            pink
177              1             0.686                   0.199            pink
178              3             0.744                   0.102     blue-purple
179              1             0.633                   0.095     blue-purple
180              1             0.658                   0.079     blue-purple
181              3             0.274                   0.074     blue-purple
182              2             0.628                   0.084     blue-purple
183              3             0.393                   0.073     blue-purple
184              1             0.344                   0.065     blue-purple
185              2             0.261                   0.053     blue-purple
186              2             0.596                   0.078     blue-purple
187              3             0.381                   0.065     blue-purple
188              2             0.317                   0.081     blue-purple
189              3             0.399                   0.086     blue-purple
190              3             0.454                    0.07     blue-purple
191              1              0.33                   0.071     blue-purple
192              2             0.312                   0.071     blue-purple
193              2             0.467                   0.081     blue-purple
194              2             0.433                   0.082     blue-purple
195              2             0.516                   0.074     blue-purple
196              3             0.373                     0.8     blue-purple
197              1              0.31                   0.054     blue-purple
198              3             0.544                   0.062     blue-purple
199              2             0.417                   0.061     blue-purple
200              2             1.835                   0.359     blue-purple
201              3             0.499                   0.065     blue-purple
202              1             0.538                   0.071     blue-purple
203              1             2.125                   0.282     blue-purple
204              1             0.379                   0.051     blue-purple
205              1              0.72                   0.092     blue-purple
206              1             0.356                   0.068     blue-purple
207              3             2.107                    0.27     blue-purple
208           <NA>              <NA>                    <NA>     blue-purple
209              3             0.737                   0.116     blue-purple
210              1             0.566                   0.083     blue-purple
211              2             0.614                    0.11     blue-purple
212              2             0.833                   0.128     blue-purple
213              3             0.645                   0.128     blue-purple
214              1             0.715                   0.122     blue-purple
215              3             0.586                   0.239           white
216              2              0.44                   0.115           white
217              3             0.503                   0.135           white
218              2             0.555                   0.191           white
219              3             0.385                    0.11           white
220              1             0.449                    0.11           white
221           <NA>              <NA>                    <NA>           white
222              1             0.429                   0.085           white
223              1             0.409                   0.111           white
224              2              0.35                    0.08           white
225              2              0.39                   0.093            blue
226              2             0.671                   0.212            blue
227              3             0.335                   0.088            blue
228              1             0.347                   0.083            blue
229              1             0.304                   0.106            blue
230              3             0.339                   0.073            blue
231              2             0.347                   0.093            blue
232              3             0.567                   0.238            blue
233              1             0.592                   0.244            blue
234           <NA>              <NA>                    <NA>            blue
235              2             1.791                   0.255     blue-purple
236              2             1.278                   0.292     blue-purple
237              1             1.893                   0.212     blue-purple
238           <NA>              <NA>                    <NA>     blue-purple
239              1             1.846                   0.167     blue-purple
240              3             1.518                   0.201     blue-purple
241              3             1.263                   0.276     blue-purple
242              1             0.936                   0.095            pink
243              2             0.859                   0.092            pink
244              3             0.868                     0.1            pink
245           <NA>              <NA>                    <NA>            pink
246              3             7.206                   0.889             red
247              3             6.892                   1.167             red
248              1             6.963                   0.901             red
249           <NA>              <NA>                    <NA>             red
250              2             7.007                    1.22             red
251              2             6.333                   0.983             red
252              3             4.593                   0.701             red
253              1             7.162                   0.943             red
254              1             6.379                   1.057             red
255              2             7.216                   0.904             red
256              2              5.56                   0.782             red
257           <NA>              <NA>                    <NA>             red
258              1             5.561                   0.792             red
259              2             5.686                   0.783             red
260              1              5.15                   0.955             red
261              3             5.995                    0.87             red
262              3             6.168                   1.003             red
263              1             5.548                   0.868            pink
264           <NA>              <NA>                    <NA>            pink
265              2             4.955                   0.842            pink
266              3             5.814                   0.717            pink
267              1             0.728                   0.096           white
268              2             0.941                   0.093           white
269           <NA>              <NA>                    <NA>           white
270              3             0.865                   0.084           white
271              1             1.945                    0.42           white
272           <NA>              <NA>                    <NA>           white
273              2             1.845                   0.277           white
274              3             1.723                   0.379           white
275              1              1.86                   0.245           white
276              2             1.729                   0.639           white
277              2             1.636                   0.336           white
278              1             1.769                   0.437           white
279              3             1.853                    0.39           white
280              3             1.852                   0.335           white
281              3             2.855                   0.804           white
282              1             2.693                   0.859           white
283              3             2.047                   0.537           white
284              2             2.948                   0.663           white
285              3             2.652                   0.761           white
286              1             2.497                   0.613           white
287              1             3.383                   0.854           white
288           <NA>              <NA>                    <NA>           white
289              2             2.897                   0.909           white
290              2             2.418                   0.585           white
291              1             3.168                   0.634           white
292              1              3.07                   0.448           white
293              2              3.94                   0.552           white
294           <NA>              <NA>                    <NA>           white
295              3             3.865                    0.56           white
296              2             3.248                   0.441           white
297              3              <NA>                   0.493           white
298              3             3.231                   0.531           white
299              2              2.81                   0.346           white
300              2              <NA>                   0.452           white
301              1              3.03                   0.447           white
302              3              <NA>                    <NA>           white
303              1              <NA>                   0.521           white
304              2             2.401                   0.437          purple
305              3               2.5                   0.325          purple
306              1             2.128                   0.433          purple
307           <NA>              <NA>                    <NA>          purple
308              1             2.209                    1.99           green
309              1             5.936                   1.356           green
310              1             5.713                   3.459            pink
311              1             6.617                   8.496            pink
312              2              6.92                    7.62            pink
313              3             5.791                   3.332            pink
314           <NA>              <NA>                    <NA>            pink
315              1             5.728                    3.35            pink
316              1             6.586                   4.074            pink
317              1             6.098                   3.502            pink
318              2             5.691                   3.516            pink
319           <NA>              <NA>                    <NA>           green
320              1             3.677                   1.625           green
321              2             4.329                   0.813           green
322              1             3.395                   0.907           green
323              1             2.685                   1.136           green
324              1             5.447                   0.809           green
325              3             2.892                   0.812           green
326              2             2.334                   1.134           green
327              1             4.775                   1.019           green
328           <NA>              <NA>                    <NA>           green
329              1             2.522                   1.465 greenish-yellow
330              1             5.465                   2.281 greenish-yellow
331           <NA>              <NA>                    <NA> greenish-yellow
332              1              5.54                   2.256 greenish-yellow
333              1             6.199                   2.988 greenish-yellow
334              1             5.559                    2.75 greenish-yellow
335              1             4.769                   1.519 greenish-yellow
336              1             5.015                   2.712 greenish-yellow
337              1             5.211                   2.459 greenish-yellow
338              2             2.939                    <NA> greenish-yellow
339              1             5.549                   2.545 greenish-yellow
340              1             2.945                   1.653 greenish-yellow
341              1             4.914                   2.748 greenish-yellow
342              1             4.117                   1.976           green
343              1             4.828                   2.232           green
344              1             4.102                   1.922           green
345              1              <NA>                    <NA>           green
346           <NA>              <NA>                    <NA>           green
347           <NA>              <NA>                    <NA>           green
348              1             6.064                   3.529           green
349              1             5.645                       3           green
350              1             4.605                   3.228           green
351              1             5.597                   2.688           green
352              2             4.186                   3.216           green
353              1             5.119                   3.057           green
354           <NA>              <NA>                    <NA>           green
355              1             5.574                   0.606           green
356              1             4.948                   0.605           green
357              1             5.903                   3.688           white
358           <NA>              <NA>                    <NA>          yellow
359              2             5.602                    2.24          yellow
360              1             5.715                   2.403          yellow
361              2             4.571                   4.459          purple
362              3             3.686                   3.091          purple
363              2             2.467                    <NA>          purple
364              1              4.52                    3.51          purple
365              1             2.845                    <NA>          purple
366           <NA>              <NA>                    <NA>          purple
367              1             5.795                   3.064          purple
368              1              5.58                   3.464           white
369           <NA>              <NA>                    <NA>           white
370              1             5.204                   2.853           white
371              2             5.784                   3.288            pink
372              1             6.239                   3.772            pink
373              1              6.03                   3.691            pink
374              1               6.2                   3.226            pink
375              3             0.862                   0.253            pink
376              1              0.96                   0.258            pink
377              2             0.867                   0.202            pink
378              2             0.867                   0.158            pink
379              2             0.967                    0.22            pink
380              3             1.104                   0.203            pink
381              1             1.106                   0.293            pink
382              1             0.961                   0.193            pink
383           <NA>              <NA>                    <NA>            pink
384              3             0.971                   0.274            pink
385              2             0.565                   0.206            pink
386              2             0.799                   0.283            pink
387              2             0.969                   0.383            pink
388              1             0.874                   0.239            pink
389              3             0.943                   0.193            pink
390              2             0.805                   0.336            pink
391           <NA>              <NA>                    <NA>            pink
392              3              0.77                   0.266            pink
393              1             0.997                   0.471            pink
394              1             0.869                   0.398            pink
395              1             0.785                   0.257            pink
396              2             0.573                   0.145          purple
397           <NA>              <NA>                    <NA>          purple
398              3             0.542                   0.092          purple
399              1             0.739                   0.162          purple
400              1             2.026                    0.38          yellow
401              2             1.004                   0.364          yellow
402              2             1.571                   0.138          yellow
403              2             1.393                   0.315          yellow
404              3             1.864                   0.159          yellow
405              2             2.137                   0.388          yellow
406              1             1.052                   0.199          yellow
407              1             2.104                   0.306          yellow
408              3             1.164                   0.306          yellow
409              1             1.173                   0.334          yellow
410              2             1.192                     0.3          yellow
411              1             1.523                   0.276          yellow
412              1             1.441                   0.258          yellow
413              3             1.579                   0.212          yellow
414              1             2.018                   0.268          yellow
415              3              1.04                   0.251          yellow
416           <NA>              <NA>                    <NA>          yellow
417              3             1.247                   0.333          yellow
418              1             1.306                   0.357          yellow
419              3             1.482                   0.239          yellow
420              3             1.842                   0.369          yellow
421              3               1.6                   0.372          yellow
422              2             1.878                   0.213          yellow
423              1             1.632                   0.318          yellow
424              2             1.399                   0.164          yellow
425              2             1.212                   0.516          yellow
426              1             0.812                   0.119       pale pink
427              1             1.265                   0.211       pale pink
428              1             0.765                   0.143       pale pink
429              3             0.806                   0.099       pale pink
430              3             1.135                   0.171       pale pink
431              3             0.569                   0.056       pale pink
432              2             1.033                   0.172       pale pink
433              3             1.291                   0.154       pale pink
434              3             0.798                   0.091       pale pink
435              1             1.023                   0.193       pale pink
436              2             0.895                   0.145       pale pink
437              2              0.88                    0.09       pale pink
438              1             1.193                   0.113       pale pink
439              3             0.883                   0.118       pale pink
440              1             0.561                   0.064       pale pink
441              3              1.06                   0.099       pale pink
442              2             0.731                   0.126       pale pink
443              1             1.195                   0.184       pale pink
444              2             1.221                   0.205       pale pink
445              1             0.794                   0.122       pale pink
446              2             0.941                   0.129       pale pink
447              2             0.779                   0.121       pale pink
448              2             0.972                   0.187       pale pink
449              2             0.881                   0.104       pale pink
450              3             0.936                   0.147       pale pink
451              2             0.801                   0.148       pale pink
452              2             0.655                   0.103       pale pink
453              3             0.768                   0.134       pale pink
454           <NA>              <NA>                    <NA>       pale pink
455              3             0.647                    0.09       pale pink
456              3             0.824                   0.118       pale pink
457              1             0.632                   0.124       pale pink
458              1             0.927                   0.145       pale pink
459              3             0.897                   0.088       pale pink
460              2             0.455                   0.061       pale pink
461              1             0.772                   0.077       pale pink
462              1             0.823                    0.15       pale pink
463              2             0.694                   0.116       pale pink
464              1             0.971                   0.112       pale pink
465              2             0.434                   0.095          purple
466              3             0.453                   0.108          purple
467              3             0.465                   0.097          purple
468              3             0.594                    0.09          purple
469              1              0.54                   0.115          purple
470              3             0.679                   0.127          purple
471              1             0.484                   0.094          purple
472              2             0.579                   0.097          purple
473              1             0.682                   0.176          purple
474              1             0.565                   0.101          purple
475              1             0.662                    0.12          purple
476              3             0.599                   0.086          purple
477              2             0.692                   0.119          purple
478              2             0.467                   0.109          purple
479              2             0.492                   0.069          purple
480              2             0.497                   0.113          purple
481              2             0.806                   0.092          purple
482              1              0.42                   0.095          purple
483              2             0.544                   0.119          purple
484              1             0.801                   0.133          purple
485              3             0.492                    0.15          purple
486              1             0.357                   0.052          purple
487              1             0.559                   0.085          purple
488              1             0.645                   0.147          purple
489              2             0.459                    0.09          purple
490              2             0.654                   0.147          purple
491              2             0.441                   0.095          purple
492              1             0.564                   0.141          purple
493              2             0.576                   0.085          purple
494              2             0.581                   0.098          purple
495              3             0.629                   0.181          purple
496              3               0.5                   0.112          purple
497              1             0.585                   0.117          purple
498              1             0.764                   0.112          purple
499              3             0.533                   0.119          purple
500              3             0.466                   0.094          purple
501              3             0.281                   0.042          purple
502              3             0.441                   0.139          purple
503           <NA>              <NA>                    <NA>          purple
504              3             0.582                   0.119          purple
505              3             1.277                   0.326     purple-blue
506              1             1.409                   0.406     purple-blue
507              2             1.467                   0.288     purple-blue
508              2             1.365                   0.306     purple-blue
509              1             1.093                   0.313     purple-blue
510           <NA>              <NA>                    <NA>     purple-blue
511              3             1.239                   0.364     purple-blue
512              3             2.335                   0.452             red
513              2             2.325                    0.46             red
514              2             2.627                   0.528             red
515           <NA>              <NA>                    <NA>             red
516              1             2.164                   0.434             red
517              2             3.116                   0.452             red
518              1             3.159                   0.617             red
519              1             2.433                   0.462             red
520              3             3.105                   0.516             red
521              3             2.774                   0.461             red
522              3             0.354                    0.08           white
523              1             0.446                   0.074           white
524              2             0.425                   0.085           white
525              1             0.517                   0.032           white
526           <NA>              <NA>                    <NA>           white
527              2             0.366                   0.049           white
528              3             0.461                   0.074           white
529              2             0.358                   0.065           white
530              3             0.367                   0.084           white
531              1             0.414                   0.096           white
532              1             0.393                   0.065           white
533              3             0.361                   0.061           white
534              2             0.473                   0.058           white
535              1             0.434                    0.09          purple
536              2             0.285                   0.087          purple
537              3             0.434                   0.087          purple
538              3             0.615                   0.061          purple
539              2              0.64                   0.083          purple
540              1             0.705                   0.081          purple
541              1             0.743                   0.074          purple
542           <NA>              <NA>                    <NA>          purple
543              3             0.722                   0.071          purple
544              2             0.653                   0.084          purple
545           <NA>              <NA>                    <NA>            pink
546              1             1.409                   0.078            pink
547              2              0.69                   0.054            pink
548              1             0.592                   0.055            pink
549              2             1.211                   0.151            pink
550              3             0.667                   0.022            pink
551              2             1.256                   0.086            pink
552              3             1.084                   0.058            pink
553              1             1.171                   0.108            pink
554           <NA>              <NA>                    <NA>            blue
555              2             1.347                   0.523            blue
556              1             1.403                   0.279            blue
557              3             2.136                   0.599            blue
558              2              1.83                   0.607            blue
559              2             2.233                   0.481            blue
560              1             1.558                   0.428            blue
561              3             1.492                   0.528            blue
562              1             2.157                   0.608            blue
563           <NA>              <NA>                    <NA>            blue
564              1             0.474                   0.132           white
565              2             0.539                   0.109           white
566              1             0.452                   0.089           white
567              2             0.618                    0.15           white
568              3              0.43                   0.134           white
569              1             0.397                   0.126           white
570              3             0.483                   0.149           white
571           <NA>              <NA>                    <NA>           white
572              1             1.235                   0.244            blue
573              1             1.249                   0.268            blue
574              2             2.251                   0.141            blue
575              1             3.222                   0.309            blue
576              1             1.292                   0.171            blue
577              1             1.674                   0.304            blue
578              3             0.409                   0.316            blue
579              3             0.713                   0.152            blue
580              3             1.186                   0.172            blue
581              2              0.68                   0.186            blue
582              3             2.819                   0.222            blue
583              2             0.856                   0.208            blue
584              2              1.33                   0.254            blue
585              1             0.528                   0.153            blue
586           <NA>              <NA>                    <NA>            blue
587              3             1.179                    0.31            blue
588              2             0.439                   0.156            blue
589              2             1.025                   0.197            blue
590              3             1.456                   0.235            blue
591              3             0.923                   0.229            blue
592              1              0.79                   0.168            blue
593              2             1.676                   0.291            blue
594           <NA>              <NA>                    <NA>            blue
595              3             1.052                   0.284            blue
596              1             0.504                   0.095            blue
597              1             0.614                   0.155            blue
598              2             1.025                   0.343            blue
599              2             0.406                   0.103            blue
600              3             0.575                   0.171            blue
601              3             0.549                   0.202            blue
602              1             1.023                   0.245            blue
603              2             0.676                    0.12            blue
604              2             0.907                   0.272     purple-blue
605              1              0.98                    0.19     purple-blue
606              2             1.075                   0.222     purple-blue
607              3             0.853                   0.303     purple-blue
608              3             0.786                   0.209     purple-blue
609              2             0.811                   0.209     purple-blue
610              2             0.781                   0.357     purple-blue
611              1             0.945                   0.208     purple-blue
612              1             0.889                   0.156     purple-blue
613              3             0.747                   0.303     purple-blue
614              3              1.15                   0.395     purple-blue
615              1             0.972                   0.312     purple-blue
616              3              1.03                   0.188     purple-blue
617              2             0.835                   0.283     purple-blue
618           <NA>              <NA>                    <NA>     purple-blue
619              1             0.832                   0.303     purple-blue
620              1             0.974                   0.371     purple-blue
621              3             1.052                   0.342     purple-blue
622              2              0.92                   0.287     purple-blue
623              3              0.72                   0.182     purple-blue
624              2             0.927                   0.342     purple-blue
625              1             0.832                    0.19     purple-blue
626              2             0.494                   0.079           white
627           <NA>              <NA>                    <NA>           white
628              1             0.654                   0.129           white
629              2             0.407                   0.091           white
630              2               0.4                   0.072           white
631              1             0.348                     0.1           white
632              3               0.4                   0.088           white
633              3             0.383                    0.11           white
634           <NA>              <NA>                    <NA>           white
635              1             0.553                   0.119           white
636              1             0.637                   0.158          yellow
637           <NA>              <NA>                    <NA>          yellow
638              3              0.64                   0.258          yellow
639              3             0.771                    0.16          yellow
640              2             0.726                   0.148          yellow
641              2             0.579                   0.281          yellow
642              1             0.828                   0.214          yellow
643              1             0.648                   0.233          yellow
644              1               1.3                   0.198            blue
645              2             1.596                   0.187            blue
646              3             1.051                   0.285            blue
647           <NA>              <NA>                    <NA>            blue
648              3             0.968                    0.17            blue
649              2             1.011                   0.165            blue
650              2             1.172                   0.163            blue
651              3             1.219                   0.259            blue
652              1             1.145                   0.224            blue
653              1             1.296                   0.235            blue
654              2             1.086                   0.192            blue
655              3             1.478                   0.222            blue
656              1             1.467                   0.245            blue
657              1             1.045                    0.24            blue
658           <NA>              <NA>                    <NA>            blue
659              1             1.112                   0.368            blue
660              2             1.168                   0.318            blue
661              3             1.259                   0.376            blue
662              2             0.916                   0.221            blue
663              3             0.898                   0.216            blue
664              1             0.636                   0.151            blue
665           <NA>              <NA>                    <NA>            blue
666              3             0.517                   0.086            blue
667              2             0.591                   0.129            blue
668           <NA>              <NA>                    <NA>            blue
669              2             0.631                   0.178            blue
670              1             0.707                   0.175            blue
671              1             0.684                   0.153            blue
672              1             1.122                   0.297            blue
673              3             0.638                   0.176            blue
674              2             1.124                   0.359            blue
675              1             1.181                   0.294            blue
676              2             0.776                   0.168            blue
677           <NA>              <NA>                    <NA>            blue
678              1             0.623                   0.351            blue
679              1             0.722                   0.255            blue
680              3             1.558                   0.212            blue
681              3             0.639                   0.176            blue
682              1             0.521                   0.267       pale blue
683              2             0.762                   0.265       pale blue
684           <NA>              <NA>                    <NA>       pale blue
685              1             1.053                   0.256       pale blue
686              2             1.117                   0.263       pale blue
687              3             1.045                   0.213       pale blue
688              3             0.808                   0.321       pale blue
689              1             0.919                    0.26       pale blue
690              2             1.732                   0.638             red
691           <NA>              <NA>                    <NA>             red
692              3              1.63                   0.586             red
693              3              1.47                   0.517             red
694              1             1.957                   0.478             red
695              2             1.462                   0.489             red
696              1             1.906                   0.461             red
697              3             1.667                   0.799             red
698              1             1.281                   0.661             red
699              2             1.985                    0.44             red
700              2             1.419                   0.226          purple
701              2             1.031                    0.23          purple
702              1               1.4                   0.251          purple
703              1             1.198                   0.292          purple
704           <NA>              <NA>                    <NA>          purple
705              3             1.219                    0.18          purple
706              3             0.823                   0.392          purple
707              1             1.004                   0.255          purple
708              2             1.127                   0.354          purple
709              3             1.127                    0.31          purple
710              3             0.796                    0.16          purple
711              3             0.566                   0.193          purple
712              2             0.679                   0.155          purple
713              1             0.804                   0.375          purple
714              2             0.795                   0.321          purple
715           <NA>              <NA>                    <NA>          purple
716              3             0.574                   0.287          purple
717              3             0.703                   0.282          purple
718              1             0.763                   0.293          purple
719              2             0.889                   0.231          purple
720              2             0.728                   0.314          purple
721              1             0.908                    0.26          purple
722              1             0.941                   0.298          purple
723           <NA>              <NA>                    <NA>          purple
724              3             0.715                   0.129          purple
725              1             0.533                   0.139          purple
726              2              0.57                   0.127          purple
727              1             0.605                   0.182          purple
728              2             0.517                   0.109          purple
729              3             0.818                   0.134          purple
730              1             1.857                   0.469            pink
731              3             1.523                   0.455            pink
732           <NA>              <NA>                    <NA>            pink
733              3             1.854                   0.296            pink
734              2             1.901                   0.493            pink
735              1             1.802                   0.261            pink
736              2             1.508                   0.235            pink
737              2             0.767                   0.207            blue
738              3             0.705                   0.182            blue
739              1             0.654                   0.281            blue
740              1             0.911                   0.242            blue
741              3             0.793                   0.256            blue
742              1              0.66                   0.322            blue
743              1             0.478                   0.149            blue
744              2             0.802                    0.12            blue
745              2             0.643                   0.207            blue
746              1             0.771                   0.111            blue
747              1             0.734                   0.223            blue
748              2             0.582                   0.137            blue
749              3             0.682                   0.284            blue
750           <NA>              <NA>                    <NA>            blue
751              3             0.553                   0.168            blue
752              2             0.668                    0.28            blue
753              3             0.433                   0.168            blue
754              3             0.479                   0.109            blue
755              2              0.47                   0.172            blue
756              3             0.885                   0.089            blue
757              2              0.54                   0.134            blue
758              2             0.639                    0.16            blue
759              1              0.52                   0.146            blue
760              1             0.625                   0.117            blue
761              3             0.479                   0.131            blue
762              3             0.831                   0.201            blue
763              3             0.752                   0.205            blue
764              1             0.852                   0.195            blue
765           <NA>              <NA>                    <NA>            blue
766              2             0.686                   0.286            blue
767              1             0.615                   0.168            blue
768              2              0.78                   0.151            blue
769              3             0.933                   0.258           white
770              2             0.947                   0.288           white
771              1             0.536                   0.191           white
772              1             0.737                   0.219           white
773           <NA>              <NA>                    <NA>           white
774              2             0.524                   0.222           white
775              3             0.697                    0.25           white
776           <NA>              <NA>                    <NA>     pale purple
777              1              0.84                   0.232     pale purple
778              3             0.721                   0.229     pale purple
779              1             0.716                   0.132     pale purple
780              2             0.823                   0.222     pale purple
781              2             0.769                   0.179     pale purple
782              3             0.794                   0.206     pale purple
783              3             0.576                   0.215     pale purple
784              1             0.798                   0.279     pale purple
785              2             0.694                   0.173     pale purple
786              3             1.148                   0.315          purple
787              2             1.048                   0.383          purple
788              1             1.025                   0.264          purple
789              3              1.05                   0.251          purple
790              1             1.018                   0.357          purple
791              2             1.183                   0.239          purple
792           <NA>              <NA>                    <NA>          purple
793              3             0.626                   0.153          purple
794              1              0.72                   0.248          purple
795              2             0.552                    0.11          purple
796              1             0.359                   0.202          purple
797              3             0.765                   0.196          purple
798              2             0.703                    0.22          purple
799              3             0.791                   0.277          purple
800           <NA>              <NA>                    <NA>          purple
801              1             0.756                   0.231          purple
802              1             0.819                   0.165          purple
803              2             0.801                   0.255          purple
804              1             0.993                   0.264          purple
805              3             0.735                    0.16          purple
806              2             0.861                   0.217          purple
807              3             0.814                   0.238          purple
808              2             0.864                   0.195          purple
809              1             0.613                   0.127          purple
810              3             0.723                   0.251          purple
811              1             0.713                   0.182          purple
812              2             0.627                   0.169          purple
813              3             0.584                   0.149          purple
814              2             0.752                   0.183          purple
815              1             0.631                    0.16          purple
816              2             0.582                   0.137          purple
817              3              0.71                   0.151          purple
818           <NA>              <NA>                    <NA>          purple
819              2             0.613                   0.137          purple
820              1             0.697                   0.193          purple
821              3             0.667                   0.179          purple
822              1             1.147                   0.182            pink
823              3             0.603                   0.216            pink
824              2             0.954                   0.123            pink
825              1             0.837                   0.335            pink
826           <NA>              <NA>                    <NA>            pink
827              3             0.658                    0.09            pink
828              2             0.913                   0.192            pink
829              2             1.147                   0.353     blue-purple
830              1             1.333                   0.372     blue-purple
831              1             1.158                   0.276     blue-purple
832              3             1.407                   0.414     blue-purple
833              2             1.125                   0.313     blue-purple
834              3             1.264                   0.359     blue-purple
835           <NA>              <NA>                    <NA>     blue-purple
836              3             1.102                   0.291     blue-purple
837              2              1.48                    0.38     blue-purple
838              1             1.345                   0.289     blue-purple
839              2             1.175                   0.385            pink
840           <NA>              <NA>                    <NA>            pink
841              1             1.241                   0.432            pink
842              3             1.081                   0.378            pink
843              3              <NA>                    <NA>          purple
844           <NA>              <NA>                    <NA>          purple
845              2             0.653                    0.13          purple
846              1             0.849                   0.172          purple
847              3             0.611                   0.149     blue-violet
848              1             0.755                   0.188     blue-violet
849           <NA>              <NA>                    <NA>     blue-violet
850              2             0.733                   0.147     blue-violet
851              1             0.902                    0.11    bluish-white
852              3             0.775                   0.137    bluish-white
853              2             0.775                   0.133    bluish-white
854           <NA>              <NA>                    <NA>    bluish-white
855              1             0.881                   0.201          purple
856              3             0.895                   0.184          purple
857           <NA>              <NA>                    <NA>          purple
858              2             0.888                   0.173          purple
859              3             1.046                   0.184          purple
860              1             0.955                   0.238          purple
861           <NA>              <NA>                    <NA>          purple
862              2             1.153                   0.191          purple
863              3             0.877                   0.239          purple
864              2             0.816                   0.203          purple
865              1              0.98                   0.202          purple
866              1             1.079                   0.202          purple
867              3             1.122                   0.443          violet
868              3             1.163                    0.28          violet
869           <NA>              <NA>                    <NA>          violet
870              1             1.295                   0.308          violet
871              2             1.115                   0.341          violet
872              1             1.039                   0.344          violet
873              2             1.109                    0.42          violet
874              2             1.336                   0.197          purple
875              3             1.348                   0.178          purple
876           <NA>              <NA>                    <NA>          purple
877              1             1.406                   0.271          purple
878              2             1.327                   0.228          purple
879              1             1.419                   0.254          purple
880              3             1.391                   0.337          purple
881              1             1.301                   0.296            pink
882              3             1.055                   0.256            pink
883              3             0.842                   0.179            pink
884              2             0.819                   0.249            pink
885           <NA>              <NA>                    <NA>            pink
886              1             1.872                   0.322            pink
887              1              0.96                   0.347            pink
888              3              0.71                   0.242            pink
889              2              1.22                   0.386            pink
890              2             1.115                   0.297            pink
891              2             1.022                   0.278            pink
892              1             0.918                   0.193            pink
893              1             0.805                    0.16            pink
894              3             1.324                   0.281            pink
895              2             0.854                    0.14            pink
896              3              1.33                   0.286            pink
897              2             1.021                   0.289            pink
898              3             0.658                    0.22            pink
899              2             0.632                   0.195            pink
900              1                 1                   0.224            pink
901              1             0.807                   0.266            pink
902              1             0.704                   0.179            pink
903              1             1.093                   0.371            pink
904              2             0.897                   0.229            pink
905              2             0.755                   0.185            pink
906              3             0.738                   0.252            pink
907              3             0.717                   0.184            pink
908           <NA>              <NA>                    <NA>            pink
909              3             1.016                   0.301            pink
910              2             0.621                   0.103            pink
911              1             0.627                    0.13            pink
912              1             2.618                    0.41            pink
913              2             1.965                   0.252            pink
914              3             0.714                   0.151            pink
915              3             0.655                   0.109            pink
916              2             1.611                   0.383            pink
917              1             1.492                    0.33            pink
918           <NA>              <NA>                    <NA>            pink
919              3              1.42                    0.35            pink
920              1             1.634                   0.405            pink
921              3             1.254                   0.282            pink
922              3             1.757                   0.218            pink
923              3              2.05                   0.381            pink
924              1             1.466                   0.391            pink
925              2             1.584                   0.244            pink
926              1             0.505                   0.116            pink
927              2             0.537                   0.099            pink
928              2              1.42                    0.32            pink
929              1             0.567                   0.224            pink
930              2             0.718                   0.195            pink
931           <NA>              <NA>                    <NA>            pink
932              3             0.596                   0.171            pink
933              1             0.643                   0.193          purple
934              1             0.964                   0.348          purple
935              2             1.162                   0.365          purple
936              3             1.171                   0.411          purple
937              1              1.17                    0.44          purple
938              1             1.453                   0.378          purple
939              2             1.128                   0.347          purple
940              2             1.068                   0.298          purple
941              3             0.924                   0.356          purple
942              3             1.248                   0.384          purple
943              2             1.226                   0.355          purple
944              2             0.915                   0.366          purple
945              3              0.75                   0.232          purple
946           <NA>              <NA>                    <NA>          purple
947              1             1.074                   0.351          purple
948              3             1.279                   0.366          purple
949              3             0.629                   0.197            blue
950              2             0.561                   0.219            blue
951              3              0.38                    0.19            blue
952           <NA>              <NA>                    <NA>            blue
953              1             0.847                   0.166            blue
954              1             0.434                   0.188            blue
955              2             0.504                   0.166            blue
956              3             0.551                   0.196            blue
957              2             0.712                    0.19            blue
958              1             0.395                   0.168            blue
959              3             0.557                   0.092          yellow
960              2             0.519                    0.07          yellow
961              1             0.888                   0.204          yellow
962              2             0.796                   0.211          yellow
963              1             0.505                   0.073          yellow
964           <NA>              <NA>                    <NA>          yellow
965              2             1.363                   0.596     blue-purple
966              1             1.237                   0.554     blue-purple
967              2             1.455                   0.664     blue-purple
968              1             1.472                   0.683     blue-purple
969              3             1.376                   0.517     blue-purple
970              2             1.603                    0.63     blue-purple
971              1             1.635                    0.52     blue-purple
972           <NA>              <NA>                    <NA>     blue-purple
973              3             1.356                   0.647     blue-purple
974              3             0.802                   0.246          purple
975              2             0.647                   0.198          purple
976              2             0.363                   0.252          purple
977              1             0.606                   0.256          purple
978              3             0.532                   0.295          purple
979              1             0.649                   0.237          purple
980              3             0.511                   0.204          purple
981              1             0.813                   0.382          purple
982              2             0.613                    0.27          purple
983              3             0.638                   0.287          purple
984           <NA>              <NA>                    <NA>          purple
985              1             0.297                   0.272          purple
986              3             0.378                   0.227          purple
987              2             0.436                   0.287          purple
988              2             0.435                   0.182          purple
989              1             0.503                   0.206          purple
990              1             0.916                   0.515            blue
991              1             0.975                   0.463            blue
992              3             0.889                   0.404            blue
993              2             0.935                   0.376            blue
994           <NA>              <NA>                    <NA>            blue
995              3             1.468                   0.375            blue
996              2             1.199                   0.436            blue
997              1              1.39                   0.564          purple
998           <NA>              <NA>                    <NA>          purple
999              2             1.067                   0.467          purple
1000             3             1.633                   0.497          purple
1001             2             0.922                   0.302          purple
1002             1             0.965                   0.398          purple
1003             3             1.396                   0.483          purple
1004             1             1.238                    0.22           white
1005             2             0.997                   0.192           white
1006             3             0.961                   0.179           white
1007          <NA>              <NA>                    <NA>           white
1008             3             0.551                   0.101           white
1009          <NA>              <NA>                    <NA>           white
1010             1             0.464                   0.107           white
1011             2             0.457                    0.11           white
1012             1             0.523                   0.159           white
1013             2             0.447                   0.116           white
1014             3             0.317                   0.103           white
1015             3             2.099                    0.31             red
1016             1             2.394                   0.344             red
1017             3             2.618                   0.343             red
1018             3             2.169                   0.375             red
1019             2             2.686                   0.316             red
1020          <NA>              <NA>                    <NA>             red
1021             2             1.836                   0.317             red
1022             2             2.074                   0.314             red
1023             1             1.898                   0.421             red
1024             1             2.389                   0.334             red
1025             3             2.485                    0.39             red
1026             1             2.233                   0.493             red
1027             3             2.332                   0.339             red
1028             1             2.093                   0.386             red
1029             3              2.32                   0.394             red
1030             2             1.706                   0.319             red
1031             1             2.297                   0.209             red
1032             3             2.361                   0.322             red
1033             2             2.658                   0.419             red
1034             1             4.108                   0.301             red
1035          <NA>              <NA>                    <NA>             red
1036             1             2.075                   0.267             red
1037             2             2.483                   0.331             red
1038             2             2.226                   0.486             red
1039             3             3.381                   0.277             red
1040             3             2.212                   0.246             red
1041             2             2.603                   0.274             red
1042             1             2.073                   0.383             red
1043             2             2.716                    0.32             red
1044             3             2.749                   0.247             red
1045             1             2.738                   0.427             red
1046             2             2.848                   0.508             red
1047             3             0.342                   0.046           white
1048             1             0.449                   0.074           white
1049             2             0.485                   0.069           white
1050             2             0.524                   0.135           white
1051             1             0.535                   0.055           white
1052             3             0.327                   0.045           white
1053             2             0.432                   0.078           white
1054             1             0.685                   0.192           white
1055             3              0.78                   0.162           white
1056          <NA>              <NA>                    <NA>           white
1057          <NA>              <NA>                    <NA>            pink
1058             1             0.894                    0.13            pink
1059             2             0.857                   0.127            pink
1060             3             1.158                   0.286            pink
1061             3             0.972                   0.186            pink
1062             2             1.297                   0.356            pink
1063             1             1.535                   0.325            pink
1064             2             0.476                   0.159      light pink
1065             1              0.61                   0.121      light pink
1066             3             0.429                   0.087      light pink
1067             1             0.592                   0.102      light pink
1068             3              0.53                    0.11      light pink
1069             2             0.604                   0.132      light pink
1070             1             0.412                    0.05      light pink
1071          <NA>              <NA>                    <NA>      light pink
1072             2             0.347                   0.076      light pink
1073             3              0.33                   0.104      light pink
1074             2             0.658                   0.134        lavender
1075             2             0.759                   0.113        lavender
1076             3              0.79                   0.108        lavender
1077             1             0.647                   0.124        lavender
1078             3             0.503                   0.103        lavender
1079             2              0.62                    0.09        lavender
1080             2             0.504                   0.146        lavender
1081             3             0.617                   0.188        lavender
1082             1             0.642                    0.21        lavender
1083             1              0.52                   0.101        lavender
1084             3             0.638                   0.179        lavender
1085             1             0.592                   0.151        lavender
1086          <NA>              <NA>                    <NA>        lavender
1087             2             1.963                   0.438      light pink
1088             1             1.784                    0.19      light pink
1089          <NA>              <NA>                    <NA>      light pink
1090             3             1.844                   0.287      light pink
1091             2             1.562                   0.162      light pink
1092             3             1.893                   0.209      light pink
1093             3             1.483                   0.212      light pink
1094             1             1.702                   0.275      light pink
1095             1             1.453                   0.243      light pink
1096             2             1.854                   0.203      light pink
1097             2             3.439                   0.197     blue-purple
1098             2             2.587                   0.192     blue-purple
1099             3             2.342                   0.259     blue-purple
1100             1              2.89                   0.172     blue-purple
1101             1             2.914                   0.239     blue-purple
1102             3             0.871                   0.383     blue-purple
1103             2             1.014                   0.416     blue-purple
1104             1             0.978                   0.414     blue-purple
1105             3             3.766                    <NA>     blue-purple
1106             3             3.106                   0.173     blue-purple
1107             2             2.157                   0.201     blue-purple
1108             1              2.42                   0.259     blue-purple
1109             1             2.092                   0.238     blue-purple
1110             2             2.315                   0.203     blue-purple
1111             3             2.344                   0.236     blue-purple
1112             1             1.519                   0.207     blue-purple
1113          <NA>              <NA>                    <NA>     blue-purple
1114             2             1.925                    0.21     blue-purple
1115             2             3.069                   0.227     blue-purple
1116             1              2.77                   0.201     blue-purple
1117             3             3.866                   0.226    light purple
1118             1             3.717                    0.25    light purple
1119             2              4.34                   0.252    light purple
1120             1             5.099                   0.365    light purple
1121             2             5.484                   0.368    light purple
1122             3             5.271                   0.299    light purple
1123             3             4.335                   0.243    light purple
1124             3              4.99                   0.259    light purple
1125          <NA>              <NA>                    <NA>    light purple
1126             3             2.837                   0.203    light purple
1127             2             3.882                   0.361    light purple
1128             3             4.157                   0.339    light purple
1129             2             2.507                   0.216    light purple
1130             1             5.339                   0.319    light purple
1131             2             4.864                   0.273    light purple
1132             1             4.693                   0.222    light purple
1133             1             3.872                   0.261    light purple
1134             1             4.339                   0.283    light purple
1135             2              4.86                   0.256    light purple
1136             1             4.344                     0.3    light purple
1137             3             3.756                   0.267    light purple
1138             2             4.744                   0.281    light purple
1139             2             1.002                   0.261          purple
1140             2             1.857                   0.203          purple
1141             3             2.139                   0.212          purple
1142             3             0.961                   0.173          purple
1143             1             1.568                   0.252          purple
1144             3             3.688                   0.202          purple
1145             2             1.102                   0.275          purple
1146             1             2.945                   0.278          purple
1147             1             2.253                   0.227          purple
1148             2              1.05                   0.157          purple
1149             2              2.58                   0.235          purple
1150             3             1.507                   0.171          purple
1151             2             2.512                    0.26          purple
1152             1             1.301                     0.2          purple
1153             1             1.022                   0.265          purple
1154             3             1.037                   0.304          purple
1155             1             2.876                   0.231          purple
1156          <NA>              <NA>                    <NA>          purple
1157             3             2.872                   0.226          purple
1158             3             2.829                   0.265          purple
1159             2              3.16                   0.254          purple
1160             1             3.105                   0.202          purple
1161          <NA>              <NA>                    <NA>          purple
1162             2             3.481                    0.32          purple
1163             3             3.806                   0.354          purple
1164             1             3.592                   0.301          purple
1165             2             0.902                   0.215          purple
1166             1             0.845                   0.175          purple
1167             3             0.911                   0.193          purple
1168             3             1.311                   0.193          purple
1169             1             0.872                   0.203          purple
1170             3             1.051                   0.209          purple
1171             1             1.192                   0.213          purple
1172             1             1.174                   0.216          purple
1173             2             0.744                   0.151          purple
1174          <NA>              <NA>                    <NA>          purple
1175             2             1.303                   0.241          purple
1176             2             1.164                   0.212          purple
1177             3             1.063                   0.264          purple
1178             3              0.89                   0.207          purple
1179             1             1.275                   0.233          purple
1180             2             1.087                   0.239          purple
1181             3             0.773                   0.227          purple
1182             1             0.915                    0.19          purple
1183             3             1.023                    0.28          purple
1184             2             1.008                   0.236          purple
1185             2             0.522                   0.151          purple
1186             1             0.902                   0.279          purple
1187             3             0.688                   0.231          purple
1188             1             1.257                   0.279          purple
1189             2             0.789                   0.207          purple
1190             2             0.882                   0.227          purple
1191             1             1.023                   0.289          purple
1192             3             0.624                   0.159          purple
1193             1             1.049                   0.248          purple
1194             1             0.907                   0.285          purple
1195             1             0.539                   0.078          purple
1196             2             0.766                   0.263          purple
1197             2             1.211                   0.279          purple
1198             1              1.18                    0.22          purple
1199             3             0.928                   0.225          purple
1200             3             0.896                   0.248          purple
1201             3             0.921                   0.264          purple
1202             2             0.912                   0.239          purple
1203             2              0.99                   0.259          purple
1204             3             1.052                   0.269          purple
1205          <NA>              <NA>                    <NA>          purple
1206             1             0.585                   0.175           white
1207             3             0.279                   0.128           white
1208             1             0.349                   0.081           white
1209             2             0.195                   0.101           white
1210             3             0.392                   0.137           white
1211             1              <NA>                    <NA>           white
1212             2             0.292                   0.085           white
1213             3              <NA>                    <NA>           white
1214             1             0.397                   0.135           white
1215             2             0.439                   0.085           white
1216             2              <NA>                    <NA>           white
1217          <NA>              <NA>                    <NA>           white
1218             3             2.968                   0.483          purple
1219             2             2.903                    0.31          purple
1220             2             2.592                   0.469          purple
1221             3             2.877                   0.295          purple
1222             2             3.067                   0.537          purple
1223             3             2.712                   0.388          purple
1224             1             3.261                   0.627          purple
1225             1             2.292                    0.41          purple
1226             1             2.839                   0.459          purple
1227          <NA>              <NA>                    <NA>          purple
1228             1             1.029                   0.167          purple
1229             1             0.712                   0.137          purple
1230          <NA>              <NA>                    <NA>          purple
1231             1             0.934                    0.17          purple
1232             2             0.854                    0.14          purple
1233             3             0.948                   0.133          purple
1234             1             1.047                   0.149          purple
1235             2             1.045                   0.156          purple
1236             3             0.884                   0.114          purple
1237             2              0.98                    0.15          purple
1238             3             1.002                   0.165          purple
1239             3             0.975                    0.13          purple
1240             1             0.892                     0.1          purple
1241             3             0.697                    0.14          purple
1242             3             1.049                   0.137          purple
1243             2             0.933                   0.134          purple
1244             2             0.887                   0.131          purple
1245             1             1.097                   0.194          purple
1246             1             1.407                   0.123          purple
1247             2             0.594                   0.151          purple
1248             2              0.89                   0.213          purple
1249             3              0.56                   0.202          purple
1250             2              0.89                   0.144           white
1251             3             0.716                   0.113           white
1252             1             0.995                   0.129           white
1253             3             0.471                   0.182           white
1254             2             1.117                   0.192           white
1255             2             0.482                   0.129           white
1256             1             0.713                    0.11           white
1257             3             1.166                   0.173           white
1258             2             1.159                   0.188           white
1259          <NA>              <NA>                    <NA>           white
1260             1             1.061                   0.168           white
1261             3             0.878                   0.119           white
1262             3             0.905                   0.115           white
1263             2              0.69                   0.099           white
1264             1              1.15                   0.168           white
1265             2              0.87                    0.16           white
1266             1             0.835                   0.164           white
1267             1             0.609                   0.124           white
1268             3             0.869                   0.258           white
1269             2             2.998                   0.491             red
1270             1             3.234                   0.522             red
1271             3             2.935                    0.51             red
1272          <NA>              <NA>                    <NA>             red
1273             3             3.216                   0.432             red
1274             2             2.683                    0.41             red
1275             1              2.71                   0.459             red
1276             1             0.474                   0.097           white
1277             2             0.675                   0.119           white
1278             3             0.514                   0.105           white
1279             2              0.39                   0.113           white
1280          <NA>              <NA>                    <NA>           white
1281             1             0.767                   0.132           white
1282             1             0.841                   0.155             red
1283             2             1.101                   0.156             red
1284             2             3.687                   0.225           white
1285             1             4.215                   0.274           white
1286             1             3.587                   0.291           white
1287             3             4.477                   0.315           white
1288             2             5.085                    0.34           white
1289          <NA>              <NA>                    <NA>           white
1290             3             3.334                   0.275           white
1291             2              4.45                   0.263          purple
1292             3             3.486                   0.253          purple
1293             2             3.544                   0.364          purple
1294             1             3.063                   0.368          purple
1295          <NA>              <NA>                    <NA>          purple
1296             1             2.731                   0.324          purple
1297             3             3.687                   0.272          purple
1298             2             2.604                   0.333          purple
1299             1             3.584                   0.332          purple
1300             2             2.197                   0.339          purple
1301             3             2.601                   0.304          purple
1302             1             4.359                   0.326          purple
1303             2             3.069                   0.366          purple
1304             3             2.194                   0.314          purple
1305             1             2.241                   0.293          purple
1306             3             4.408                   0.315          purple
1307             3             0.809                   0.193           white
1308             1             0.799                   0.137           white
1309             2             0.682                   0.145           white
1310             3             0.715                   0.147           white
1311             1             0.972                   0.258           white
1312             2             0.941                   0.188           white
1313          <NA>              <NA>                    <NA>           white
1314             3             1.265                   0.261            blue
1315             2             1.006                   0.124            blue
1316          <NA>              <NA>                    <NA>            blue
1317             1             1.025                   0.387            blue
1318             3             0.727                   0.192            blue
1319             2              0.98                   0.241            blue
1320             2             0.994                   0.327            blue
1321             3             0.791                   0.204            blue
1322             3             0.993                   0.275            blue
1323             1             0.899                   0.149            blue
1324             2             0.926                   0.214            blue
1325             1             1.087                   0.127            blue
1326             1             0.833                   0.174            blue
1327             2              1.82                   0.385           white
1328             2             1.249                   0.196           white
1329             1             1.403                   0.391           white
1330             3             1.065                   0.438           white
1331             3             1.071                   0.303           white
1332             2              1.46                   0.401           white
1333             3             1.418                   0.316           white
1334             1             0.879                   0.279           white
1335             1             1.928                   0.428           white
1336          <NA>              <NA>                    <NA>           white
1337          <NA>              <NA>                    <NA>           white
1338             3             1.026                   0.188           white
1339             3             1.011                   0.306           white
1340             2             1.213                   0.293           white
1341             2             0.806                   0.234           white
1342             1             1.018                   0.183           white
1343             2              1.07                   0.334           white
1344             1              0.79                   0.306           white
1345             3             1.219                   0.231           white
1346             1             0.967                   0.293           white
1347             3             0.942                   0.288           white
1348             2              1.13                   0.324           white
1349             1             1.013                   0.235           white
1350             1             0.977                   0.178           white
1351             1             0.269                   0.096           white
1352             3             0.272                   0.105           white
1353             2             0.242                   0.106           white
1354             2             0.288                   0.097           white
1355             2             0.357                   0.156           white
1356             1              0.27                   0.102           white
1357             3             0.279                   0.102           white
1358             1             0.319                   0.114           white
1359          <NA>              <NA>                    <NA>           white
1360             3             0.237                   0.094           white
1361             2             1.872                   0.217          yellow
1362             1             1.876                   0.193          yellow
1363             3             1.878                   0.179          yellow
1364             2             1.175                   0.163          yellow
1365             1             1.691                   0.246          yellow
1366             1             1.508                   0.254          yellow
1367          <NA>              <NA>                    <NA>          yellow
1368             3             1.497                   0.271          yellow
1369             2             1.832                   0.202          yellow
1370             3             1.889                   0.216          yellow
1371             2             1.425                   0.346          purple
1372             3             1.642                   0.266          purple
1373             1             1.529                    0.25          purple
1374             1             1.594                   0.282          purple
1375          <NA>              <NA>                    <NA>          purple
1376             3              1.27                   0.201          purple
1377             2             1.477                   0.254          purple
1378             2             2.644                    0.21           white
1379             1             2.223                   0.418           white
1380             1             2.863                   0.289           white
1381          <NA>              <NA>                    <NA>           white
1382             3             2.616                   0.271           white
1383             2             2.602                   0.248           white
1384             1             1.855                   0.148           white
1385             2             2.158                   0.464           white
1386             3             2.261                    0.42           white
1387             3             2.084                    0.19           white
1388             3             1.141                   0.231          yellow
1389             1             0.961                   0.266          yellow
1390             1              1.05                    0.21          yellow
1391             2             1.113                   0.449          yellow
1392          <NA>              <NA>                    <NA>          yellow
1393             3             0.909                   0.297          yellow
1394             2             0.911                   0.366          yellow
1395             1             2.102                   0.221            pink
1396             1             1.992                    0.18            pink
1397          <NA>              <NA>                    <NA>            pink
1398             3             1.955                   0.161            pink
1399             3             0.886                   0.205            pink
1400             3             1.764                   0.163            pink
1401             1             1.765                   0.254            pink
1402             2             1.601                   0.281            pink
1403             2             0.953                   0.141            pink
1404             1             1.035                   0.141            pink
1405             2             1.407                   0.184            pink
1406             2             1.779                   0.186            pink
1407             3             1.378                   0.147            pink
1408             3             0.944                   0.098           white
1409             2             0.779                   0.142           white
1410             1             0.899                   0.096           white
1411          <NA>              <NA>                    <NA>           white
1412             3               2.3                   0.353           white
1413             2             2.007                   0.383           white
1414             1             2.102                   0.246           white
1415          <NA>              <NA>                    <NA>           white
1416             3             1.062                   0.207            pink
1417             2             0.662                   0.101            pink
1418             3             1.179                   0.104            pink
1419             1             0.832                   0.198            pink
1420             3             0.529                   0.101            pink
1421             1             1.117                   0.194            pink
1422             1             1.028                   0.202            pink
1423             3             0.844                   0.137            pink
1424             2             0.899                   0.125            pink
1425             3             0.697                   0.291            pink
1426          <NA>              <NA>                    <NA>            pink
1427             2              1.03                   0.143            pink
1428             2             0.794                   0.226            pink
1429             1             1.048                   0.103            pink
1430             2             1.113                   0.115            pink
1431             1             0.819                   0.121            pink
1432             3             0.461                   0.187          purple
1433             2             0.671                   0.244          purple
1434             1             0.647                   0.265          purple
1435          <NA>              <NA>                    <NA>          purple
1436             1             0.489                   0.227          purple
1437             3             0.752                   0.214          purple
1438             2             0.539                   0.213          purple
1439             2             1.145                    0.27           white
1440             1              1.36                   0.288           white
1441             3             1.259                   0.246           white
1442          <NA>              <NA>                    <NA>           white
1443             2             2.994                   0.353          purple
1444          <NA>              <NA>                    <NA>          purple
1445             1             2.765                   0.326          purple
1446             2             2.264                   0.341          purple
1447             3             2.482                    0.32          purple
1448             1             2.142                   0.392          purple
1449             3             2.551                   0.464          purple
1450             1               0.4                   0.181           white
1451             1             0.459                   0.188           white
1452             3              <NA>                    <NA>           white
1453             2              0.55                   0.131           white
1454             2             0.433                   0.165           white
1455             3             0.451                     0.2           white
1456             3             0.451                   0.168           white
1457          <NA>              <NA>                    <NA>            pink
1458             3              2.36                   0.169            pink
1459             2             2.248                   0.234            pink
1460             3              2.37                   0.254            pink
1461             1             2.635                   0.178            pink
1462             2             2.973                   0.224            pink
1463             1             2.962                   0.229            pink
1464             1             0.681                   0.215           white
1465             1             1.114                   0.336           white
1466          <NA>              <NA>                    <NA>           white
1467             2             0.806                   0.235           white
1468             3             0.615                   0.163           white
1469             3             1.083                   0.259           white
1470             3             0.963                   0.339           white
1471             1              0.78                   0.272           white
1472             2             0.671                   0.184           white
1473             2             0.843                   0.364           white
1474             3               0.6                   0.172           white
1475             2             0.515                   0.175           white
1476          <NA>              <NA>                    <NA>           white
1477             1             0.561                   0.199           white
1478          <NA>              <NA>                    <NA>           white
1479             3             0.806                   0.265           white
1480             1             1.365                   0.444           white
1481             3             1.152                   0.353           white
1482             2             0.945                   0.303           white
1483             1              1.31                   0.316           white
1484             2             0.806                   0.183           white
1485             2             2.392                   0.422            pink
1486             1             1.865                   0.354            pink
1487             2             2.162                   0.323            pink
1488             3             1.837                   0.258            pink
1489             3             1.837                   0.357            pink
1490             1             2.685                   0.396            pink
1491          <NA>              <NA>                    <NA>            pink
1492             3             1.615                   0.317           white
1493          <NA>              <NA>                    <NA>           white
1494             2             1.607                   0.274           white
1495             1             1.433                   0.258           white
1496             2               1.4                   0.247           white
1497             1             1.591                    0.29           white
1498             1             1.331                   0.283           white
1499             3             1.096                   0.253           white
1500             3             1.296                   0.254           white
1501             2              1.31                   0.243           white
1502             2              1.12                   0.212           white
1503             1             1.244                    0.34           white
1504             3             1.566                   0.241           white
1505          <NA>              <NA>                    <NA>           white
1506             2             1.444                     0.2           white
1507             3             1.467                   0.288           white
1508             1             1.356                   0.246           white
1509             1             0.418                   0.134           white
1510             3             0.493                    0.11           white
1511             2             0.432                    0.13           white
1512          <NA>              <NA>                    <NA>           white
1513          <NA>              <NA>                    <NA>           white
1514             3             1.643                   0.312           white
1515             2             1.246                   0.192           white
1516             2             1.907                   0.307           white
1517             1             1.821                   0.322           white
1518             3             1.238                   0.194           white
1519             1              1.26                   0.245           white
1520             2             3.088                    0.23           white
1521             2             1.574                   0.216           white
1522             1              1.81                   0.225           white
1523          <NA>              <NA>                    <NA>           white
1524             1              2.93                   0.183           white
1525             3             1.087                   0.191           white
1526             3             2.834                   0.302           white
1527             1             0.348                   0.186           white
1528             1             0.476                   0.178           white
1529             3             0.359                    0.11           white
1530             3             0.437                   0.184           white
1531             2             0.422                   0.134           white
1532             2             0.387                   0.191           white
1533             1             0.394                   0.146           white
1534          <NA>              <NA>                    <NA>           white
1535             1             0.546                   0.112           white
1536             3             0.369                    0.11           white
1537             2             0.496                   0.207           white
1538             2             0.421                   0.208           white
1539             3             0.416                   0.133           white
1540             3             0.685                   0.372            pink
1541          <NA>              <NA>                    <NA>            pink
1542             1             0.873                   0.265            pink
1543             2             0.561                   0.288            pink
1544             1             0.723                   0.357            pink
1545             3             0.952                   0.361            pink
1546             2             0.812                   0.292            pink
1547             2             1.316                   0.166           white
1548             1             1.369                   0.213           white
1549             1             1.256                   0.188           white
1550             3               1.4                   0.179           white
1551             2             1.322                   0.201           white
1552          <NA>              <NA>                    <NA>           white
1553             2             0.699                   0.096           white
1554             3             1.182                   0.146           white
1555             2             0.866                   0.134           white
1556          <NA>              <NA>                    <NA>           white
1557             1             0.797                   0.193           white
1558             1             0.971                   0.159           white
1559             1             0.842                   0.129           white
1560             3             0.872                   0.146           white
1561             3             0.733                   0.124           white
1562             2             1.132                    0.14           white
1563          <NA>              <NA>                    <NA>            pink
1564             1             2.941                   0.465            pink
1565             2             2.624                   0.481            pink
1566             1             2.736                   0.465            pink
1567             2             2.141                   0.291            pink
1568             2             2.427                    0.37            pink
1569             1              2.64                   0.283            pink
1570             3              2.15                    0.25            pink
1571             1             2.937                   0.391            pink
1572             3             3.217                   0.351            pink
1573             3             3.068                   0.311            pink
1574             3             2.186                   0.287            pink
1575             2             2.265                   0.256            pink
1576             1             2.781                    0.39            pink
1577             3             2.596                   0.304            pink
1578             3             1.851                   0.341            pink
1579             2             3.576                   0.341            pink
1580             1             3.364                   0.314            pink
1581             2             3.151                   0.378            pink
1582             3             0.851                   0.548           white
1583             1             0.913                   0.356           white
1584             2             1.139                     0.6           white
1585             2             0.735                   0.368           white
1586             1             1.244                   0.532           white
1587             1             0.849                   0.325           white
1588          <NA>              <NA>                    <NA>           white
1589             2             1.071                   0.492           white
1590             3             0.882                   0.359           white
1591             3             0.803                   0.227           white
1592             3              0.82                   0.514           white
1593             2             0.814                   0.347           white
1594             1             0.802                   0.382           white
1595             2             1.095                   0.308           white
1596             2             1.863                   0.405           white
1597             1             1.386                   0.404           white
1598          <NA>              <NA>                    <NA>           white
1599             3             1.111                   0.377           white
1600             1             1.208                   0.337           white
1601             3             1.579                   0.332           white
1602             3             0.862                   0.358           white
1603             2             0.795                   0.393           white
1604          <NA>              <NA>                    <NA>           white
1605             1              0.91                   0.313           white
1606             2             0.665                   0.158           white
1607             3              0.59                    0.32           white
1608             1             0.642                   0.308           white
1609          <NA>              <NA>                    <NA>            pink
1610             1             1.976                   0.483            pink
1611             3             1.867                   0.355            pink
1612             1             1.582                    0.42            pink
1613             2             1.553                   0.376            pink
1614             3              1.68                   0.371            pink
1615             1             1.999                   0.542            pink
1616             2             1.877                   0.398            pink
1617             2             1.974                   0.434            pink
1618             3             1.615                   0.394            pink
1619             1             2.471                   0.307           white
1620             3              2.06                   0.276           white
1621             3             2.443                   0.324           white
1622             3             2.579                   0.617           white
1623             2             2.885                   0.541           white
1624             1             2.423                   0.237           white
1625             3             2.293                   0.431           white
1626             2             2.879                   0.343           white
1627             2             2.172                   0.359           white
1628             1             2.139                   0.384           white
1629             2             2.745                   0.397           white
1630             1             2.284                    0.27           white
1631          <NA>              <NA>                    <NA>           white
1632             2             0.389                    0.23          yellow
1633             2             0.471                    0.28          yellow
1634             1             0.541                   0.215          yellow
1635             2             0.307                   0.149          yellow
1636             3             0.498                   0.188          yellow
1637             1             0.361                   0.186          yellow
1638             3             0.362                   0.147          yellow
1639          <NA>              <NA>                    <NA>          yellow
1640             3              0.46                    0.13          yellow
1641             1             0.357                   0.114          yellow
1642          <NA>              <NA>                    <NA>           white
1643             1             0.695                   0.411           white
1644             2             0.527                   0.322           white
1645             3             0.552                   0.423           white
1646             1             0.536                   0.306           white
1647             3             0.626                   0.379           white
1648             1             0.557                   0.219           white
1649             3             0.374                   0.281           white
1650             2             0.617                    0.24           white
1651             2             0.688                   0.374           white
1652          <NA>              <NA>                    <NA>           white
1653             3             1.652                   0.179           white
1654             1             1.547                   0.207           white
1655             2             1.754                   0.188           white
1656             1             1.287                   0.194           white
1657             3             1.414                   0.242           white
1658          <NA>              <NA>                    <NA>           white
1659             2              1.56                   0.231           white
1660          <NA>              <NA>                    <NA>           white
1661             3             0.365                    <NA>           white
1662             2             0.326                   0.159           white
1663             1              0.39                   0.172           white
1664             3             0.925                   0.212           white
1665          <NA>              <NA>                    <NA>           white
1666             2             1.336                   0.296           white
1667             1             1.438                   0.386           white
1668             1             0.931                   0.243            pink
1669             2             1.048                    0.24            pink
1670          <NA>              <NA>                    <NA>            pink
1671             3             1.114                    0.28            pink
1672             1             1.355                   0.357     blue-purple
1673          <NA>              <NA>                    <NA>     blue-purple
1674             3             1.589                   0.855     blue-purple
1675             1             1.812                   0.568     blue-purple
1676             2             1.659                   0.594     blue-purple
1677             2              1.36                   0.364     blue-purple
1678             3              1.05                   0.351     blue-purple
1679             2             1.068                   0.186           white
1680             3             1.096                   0.168           white
1681             2              1.09                   0.181           white
1682             1             1.409                    0.21           white
1683          <NA>              <NA>                    <NA>           white
1684             1             1.605                    0.35           white
1685             3             1.333                   0.269           white
1686             3              1.33                   0.262           white
1687             2             1.855                   0.267           white
1688             1             1.573                     0.2           white
1689             3             0.565                   0.169           white
1690             1             0.533                   0.119           white
1691          <NA>              <NA>                    <NA>           white
1692             2             0.486                    0.11           white
1693             1             1.878                   0.326           white
1694             3             1.784                     0.3           white
1695             2             2.261                    0.29           white
1696             3             1.566                    0.21           white
1697             1             2.297                   0.314           white
1698             1             1.782                   0.216           white
1699             2             1.885                   0.263           white
1700          <NA>              <NA>                    <NA>           white
1701             2             1.411                   0.223           white
1702             1             1.911                   0.206           white
1703             2              3.02                   0.346           white
1704             3             2.538                   0.203           white
1705             3             2.154                   0.227           white
1706             1             1.425                   0.258            pink
1707             1             1.171                   0.265            pink
1708             3              1.15                   0.371            pink
1709          <NA>              <NA>                    <NA>            pink
1710             3             1.749                   0.279            pink
1711             2              1.18                   0.248            pink
1712             2             1.684                   0.194            pink
1713             1              0.99                    0.22          yellow
1714             2             0.607                   0.318          yellow
1715             2              1.15                   0.151          yellow
1716             3             0.817                   0.257          yellow
1717          <NA>              <NA>                    <NA>          yellow
1718             1             0.649                   0.239          yellow
1719             3             0.846                   0.205          yellow
1720             1             0.757                   0.312          yellow
1721             2             0.804                   0.245          yellow
1722             2             0.981                   0.204  pinkish purple
1723          <NA>              <NA>                    <NA>  pinkish purple
1724             1             1.372                   0.277  pinkish purple
1725             2             1.376                   0.228  pinkish purple
1726             3             0.809                   0.189  pinkish purple
1727             2             0.919                   0.163  pinkish purple
1728             1             0.668                   0.261  pinkish purple
1729             1              0.93                   0.169  pinkish purple
1730             3             0.914                   0.206  pinkish purple
1731             3             1.242                   0.339  pinkish purple
1732             2             1.268                   0.243  pinkish purple
1733          <NA>              <NA>                    <NA>  pinkish purple
1734             1             1.481                   0.161  pinkish purple
1735             2             0.496                   0.174  pinkish purple
1736             3             0.487                   0.114  pinkish purple
1737             3             1.396                   0.152  pinkish purple
1738             1             0.548                   0.096  pinkish purple
1739             2             2.684                    0.16           white
1740             3             2.273                   0.269           white
1741          <NA>              <NA>                    <NA>           white
1742             1             1.703                   0.236           white
1743             1             1.973                   0.186           white
1744             3             2.034                   0.197           white
1745             2             2.221                   0.324           white
1746             1             1.295                   0.254            pink
1747             1             0.862                   0.223            pink
1748             2             0.563                   0.124            pink
1749             2             0.705                   0.305            pink
1750             2             0.832                   0.224            pink
1751             2              0.84                   0.321            pink
1752             1               0.6                   0.119            pink
1753             1             0.781                   0.205            pink
1754             1             1.289                   0.209            pink
1755             2             1.223                   0.195            pink
1756             1              0.68                   0.275            pink
1757             3             0.905                   0.265            pink
1758          <NA>              <NA>                    <NA>            pink
1759             2              0.98                   0.273            pink
1760             3             0.736                   0.209            pink
1761             3             1.025                   0.185            pink
1762             1             1.041                   0.239          purple
1763             1             0.839                   0.193          purple
1764             2             0.865                   0.246          purple
1765             2             0.841                   0.321          purple
1766             3             0.698                   0.272          purple
1767             2             0.764                   0.174          purple
1768          <NA>              <NA>                    <NA>          purple
1769             3             0.815                   0.135          purple
1770             1              0.93                   0.194          purple
1771          <NA>              <NA>                    <NA>             red
1772             2             1.613                   0.277             red
1773             3             1.744                   0.396             red
1774             1             1.946                   0.267             red
1775             3             0.609                    0.19          purple
1776             2             0.781                   0.139          purple
1777             3             0.946                   0.187          purple
1778          <NA>              <NA>                    <NA>          purple
1779             1             0.932                   0.138          purple
1780             3             1.152                   0.107          purple
1781             3               1.1                     0.2          purple
1782             2             0.994                   0.197          purple
1783             2             1.388                   0.271          purple
1784             1             1.243                    0.16          purple
1785             2             1.072                   0.158          purple
1786             2             1.323                   0.194          purple
1787             1             0.994                   0.257          purple
1788             1             1.366                   0.253          purple
1789             3             0.886                   0.181          purple
1790             1             1.033                   0.177          purple
1791             2             1.583                   0.307          purple
1792             1             1.696                   0.536          purple
1793          <NA>              <NA>                    <NA>          purple
1794             3             1.788                   0.285          purple
1795             3             1.262                   0.351          purple
1796             2             1.679                    0.44          purple
1797             1             1.353                   0.307          purple
1798             3             2.809                   0.347          yellow
1799             2             2.718                   0.354          yellow
1800          <NA>              <NA>                    <NA>          yellow
1801             1             2.696                   0.407          yellow
1802             1             1.305                   0.283     pale violet
1803             2             1.944                    0.21     pale violet
1804          <NA>              <NA>                    <NA>     pale violet
1805          <NA>              <NA>                    <NA>            blue
1806             1              0.86                   0.233            blue
1807             2               0.9                   0.141            blue
1808             1             0.542                   0.228            blue
1809             3             0.645                   0.203            blue
1810             1             0.592                   0.118           white
1811          <NA>              <NA>                    <NA>           white
1812             3              0.47                   0.138           white
1813             1               0.6                   0.204           white
1814             3              0.62                   0.099           white
1815             2             0.417                   0.134           white
1816             2             0.607                   0.096           white
1817             2             0.875                   0.204            pink
1818             1             0.933                   0.136            pink
1819             3             1.112                   0.161            pink
1820             2             0.884                   0.205            pink
1821             3             1.455                   0.211            pink
1822             3             0.905                   0.148            pink
1823             1             1.322                   0.244            pink
1824             2              1.19                   0.156            pink
1825             3             0.834                   0.117            pink
1826          <NA>              <NA>                    <NA>            pink
1827             2             0.803                   0.137            pink
1828             2             0.654                   0.126            pink
1829             1             0.679                     0.2            pink
1830             1             0.831                   0.135            pink
1831             1             0.884                   0.147            pink
1832             3             0.873                   0.133            pink
1833             1             0.834                   0.221            pink
1834             2             0.993                   0.191            pink
1835             2              0.44                   0.164            pink
1836             3             0.692                   0.233            pink
1837             2             0.575                   0.225            pink
1838             1             0.604                   0.158            pink
1839          <NA>              <NA>                    <NA>            pink
1840             1             0.843                   0.196            pink
1841             3             1.179                   0.147            pink
1842             3             0.584                   0.106      light blue
1843             2             0.501                   0.108      light blue
1844          <NA>              <NA>                    <NA>      light blue
1845             1             0.464                    0.11      light blue
1846             1             0.522                   0.099            pink
1847             2             0.457                   0.127            pink
1848             3             0.474                   0.079            pink
1849          <NA>              <NA>                    <NA>            pink
1850             2             0.829                   0.129            pink
1851             2             0.459                   0.096            pink
1852             3             0.833                   0.132            pink
1853             1             0.551                   0.126            pink
1854             1             0.675                   0.099            pink
1855             1             0.272                    0.05            pink
1856             3             0.358                   0.073            pink
1857             2             0.361                   0.093            pink
1858             3             0.339                   0.118            pink
1859             2             0.532                   0.109            pink
1860             1             0.547                   0.116            pink
1861             1              0.51                   0.053          purple
1862             2             0.449                    0.06          purple
1863          <NA>              <NA>                    <NA>          purple
1864             3             0.486                   0.067          purple
1865             2             0.352                   0.117          yellow
1866             1             0.338                   0.079          yellow
1867             1             0.465                   0.069          yellow
1868          <NA>              <NA>                    <NA>          yellow
1869             2             0.351                   0.059          yellow
1870             3              0.39                   0.098          yellow
1871             2             0.707                   0.133           white
1872             3             0.695                   0.108           white
1873             1             0.862                   0.132           white
1874          <NA>              <NA>                    <NA>           white
1875             3             0.581                    0.11           white
1876             2             0.652                   0.119           white
1877             1             0.784                   0.101           white
1878             1             0.428                   0.083           white
1879             3             0.391                   0.071           white
1880             3             0.341                   0.086           white
1881          <NA>              <NA>                    <NA>           white
1882             2             0.438                   0.094           white
1883             2             0.306                    0.07           white
1884             1             0.324                   0.049           white
1885          <NA>              <NA>                    <NA>          purple
1886             3             0.299                   0.064          purple
1887             1             0.526                   0.051          purple
1888             2             0.355                   0.075          purple
1889             3             0.411                   0.132           white
1890             1             0.277                   0.057           white
1891          <NA>              <NA>                    <NA>           white
1892             2             0.289                   0.087           white
1893             1             0.461                   0.112          purple
1894             2             0.351                   0.136          purple
1895          <NA>              <NA>                    <NA>          purple
1896             2             0.331                   0.051           white
1897          <NA>              <NA>                    <NA>           white
1898             1             0.367                   0.044           white
1899             3             0.235                   0.053           white
1900             3             0.362                   0.061     blue-purple
1901             2             0.448                   0.056     blue-purple
1902             1             0.424                   0.056     blue-purple
1903          <NA>              <NA>                    <NA>     blue-purple
1904             2             0.454                     0.1           white
1905          <NA>              <NA>                    <NA>           white
1906             1             0.522                   0.175           white
1907             3             0.365                   0.111           white
1908             1             0.401                   0.113          purple
1909          <NA>              <NA>                    <NA>          purple
1910             3             0.358                   0.093          purple
1911             2              0.32                   0.056          purple
1912             1             0.394                   0.086          purple
1913             3             0.996                   0.183            pink
1914             1             1.142                   0.254            pink
1915             1              1.27                   0.215            pink
1916             2              1.14                   0.166            pink
1917             2             0.858                   0.119            pink
1918             3             1.345                   0.263            pink
1919             2             0.833                   0.214            pink
1920             1             1.585                   0.233            pink
1921          <NA>              <NA>                    <NA>            pink
1922             3             1.068                   0.193            pink
1923             2             1.411                   0.259            pink
1924             3             1.287                   0.211            pink
1925             1             0.951                   0.156            pink
1926             1             0.611                    0.08            blue
1927          <NA>              <NA>                    <NA>            blue
1928             2             0.502                    0.08            blue
1929             3             0.386                   0.129            blue
1930             2             0.543                   0.109           white
1931          <NA>              <NA>                    <NA>           white
1932             3             0.464                   0.116           white
1933             1             0.716                   0.151           white
1934             1             0.601                   0.137          purple
1935             3             0.697                   0.182          purple
1936             2              0.58                   0.152          purple
1937             2             0.422                   0.129          purple
1938             1             0.539                   0.126          purple
1939          <NA>              <NA>                    <NA>          purple
1940             3             0.499                   0.145          purple
1941          <NA>              <NA>                    <NA>           white
1942             2             0.459                   0.101           white
1943             3              0.46                   0.158           white
1944             1             0.537                   0.088           white
1945             1             0.374                    0.04           white
1946             1             0.658                   0.095            pink
1947             3             0.306                   0.165            pink
1948             2             0.306                   0.147            pink
1949             3             0.532                   0.089            pink
1950             2             0.555                   0.102            pink
1951             3             0.512                   0.078            pink
1952             2             0.472                   0.083            pink
1953             1              0.37                   0.127            pink
1954          <NA>              <NA>                    <NA>            pink
1955             1             0.388                   0.083            pink
1956             3             0.629                    0.17            blue
1957             2             0.698                   0.067            blue
1958             3             0.603                   0.092            blue
1959             2             0.662                    0.13            blue
1960          <NA>              <NA>                    <NA>            blue
1961             3             0.547                   0.142            blue
1962             1              0.71                   0.107            blue
1963             2             0.758                   0.115            blue
1964             1             0.761                   0.148            blue
1965             1             0.927                   0.165            blue
1966             2             0.763                   0.128            blue
1967             1             0.682                   0.112            blue
1968             2              0.57                   0.148           white
1969             2             0.692                   0.168           white
1970             1             0.968                   0.127           white
1971             1             0.596                   0.122           white
1972          <NA>              <NA>                    <NA>           white
1973          <NA>              <NA>                    <NA>          purple
1974             2             0.552                   0.162          purple
1975             3             0.803                    0.18          purple
1976             2             0.712                   0.125          purple
1977             3              0.62                   0.102          purple
1978             1             0.792                   0.147          purple
1979             1             0.633                   0.108          purple
1980             2             0.949                   0.179          purple
1981             3             1.001                   0.196          purple
1982             1              1.05                   0.152          purple
1983             3             0.805                    0.21          purple
1984             1             0.962                   0.266          purple
1985          <NA>              <NA>                    <NA>          purple
1986             2             0.737                   0.237          purple
1987             1             1.833                     0.2          purple
1988             2             2.344                   0.222          purple
1989             2             1.452                    0.15          purple
1990             3             1.542                   0.277          purple
1991          <NA>              <NA>                    <NA>          purple
1992             3             1.706                    0.16          purple
1993             1              1.98                   0.258          purple
1994             1             2.598                   0.217            pink
1995          <NA>              <NA>                    <NA>            pink
1996             2             2.441                   0.222            pink
1997             2             2.619                   0.306          purple
1998          <NA>              <NA>                    <NA>          purple
1999             1             2.888                   0.346          purple
2000             3             2.478                   0.208          purple
2001             2              2.18                   0.159          purple
2002             1             2.447                   0.174          purple
2003             3             2.148                   0.215          purple
2004             1             1.702                    0.12          purple
2005             2             2.632                   0.209          purple
2006             2             1.528                   0.171          purple
2007             1              1.97                   0.192          purple
2008             1             2.305                   0.179          purple
2009             2             2.281                   0.211          purple
2010             2              1.96                   0.181          purple
2011             3              1.62                   0.169          purple
2012             2             1.758                   0.317          purple
2013             3             1.608                    0.16          purple
2014             2             2.392                   0.205          purple
2015             1             2.859                    0.24          purple
2016             3             1.782                   0.132          purple
2017             2             1.855                   0.188          purple
2018          <NA>              <NA>                    <NA>          purple
2019             1             1.685                   0.199          purple
2020             1             2.087                   0.179          purple
2021             3             1.516                   0.168          purple
2022             3             2.009                    0.18          purple
2023             3             1.767                   0.233          purple
2024             1             2.162                   0.196          purple
2025             3             1.427                   0.137          purple
2026             2             1.201                   0.112            pink
2027             1             1.957                   0.185            pink
2028          <NA>              <NA>                    <NA>            pink
2029             2             1.898                   0.264           white
2030             3             1.753                   0.224           white
2031          <NA>              <NA>                    <NA>           white
2032             2             1.661                   0.192           white
2033             1             1.802                   0.176           white
2034             3             1.982                   0.226           white
2035             1             1.981                   0.215           white
2036             2             2.084                   0.209           white
2037             3             1.916                   0.223           white
2038             1             2.393                   0.241           white
2039          <NA>              <NA>                    <NA>           white
2040             3              1.38                   0.181           white
2041             1             1.797                    0.21           white
2042             2             1.289                   0.204           white
2043             2             1.543                   0.199           white
2044             3             1.434                    0.18           white
2045             3             1.611                   0.191           white
2046             1             1.292                   0.314           white
2047             2             1.473                   0.275           white
2048             1             1.664                   0.159           white
2049             3             1.572                   0.187          purple
2050             1             1.759                   0.202          purple
2051             2             1.863                   0.187          purple
2052          <NA>              <NA>                    <NA>          purple
2053             3             1.705                   0.213          purple
2054             2              1.98                   0.251          purple
2055             1             2.254                   0.213          purple
2056          <NA>              <NA>                    <NA>          purple
2057             3             3.436                   0.285          purple
2058             1             3.411                   0.315          purple
2059             2             2.194                   0.273          purple
2060             1             2.224                   0.251          purple
2061             2             2.572                    0.28          purple
2062             3             2.357                   0.285          purple
2063             3             1.336                   0.138          purple
2064          <NA>              <NA>                    <NA>          purple
2065             2             1.756                   0.174          purple
2066             2             1.215                   0.151          purple
2067             3             2.083                   0.173          purple
2068             1             1.539                   0.191          purple
2069             1             1.171                   0.147          purple
2070             1             4.118                   0.365          purple
2071             2             3.539                   0.252          purple
2072             3             3.408                   0.354          purple
2073             2              3.17                   0.157          purple
2074             1             3.679                   0.359          purple
2075             1             2.501                   0.159          purple
2076             3             2.293                   0.208          purple
2077          <NA>              <NA>                    <NA>          purple
2078             2              3.85                   0.309          purple
2079             3             3.081                   0.297          purple
2080             1             2.536                   0.299          purple
2081          <NA>              <NA>                    <NA>          purple
2082             2             2.141                   0.263          purple
2083             3             2.414                    <NA>          purple
2084             1             2.112                   0.236            pink
2085             2             1.995                    0.18            pink
2086             3             1.955                   0.286            pink
2087          <NA>              <NA>                    <NA>            pink
2088             1             2.171                   0.294            pink
2089             2             2.107                   0.213            pink
2090             2             0.947                   0.166           white
2091             1             0.979                   0.146           white
2092          <NA>              <NA>                    <NA>           white
2093             3             0.973                   0.161           white
2094             1             2.203                   0.194          purple
2095             1             1.634                   0.179          purple
2096             1             2.166                   0.119          purple
2097             2             2.072                   0.157          purple
2098             3             1.345                   0.179          purple
2099             3             1.534                   0.222          purple
2100             1              1.88                   0.236          purple
2101             2             1.853                   0.187          purple
2102             2             1.742                   0.191          purple
2103             3             1.554                   0.199          purple
2104             3             1.482                   0.125          purple
2105             3             2.233                   0.172          purple
2106             2             1.595                   0.165          purple
2107             2             1.382                   0.168          purple
2108             1             1.463                   0.124          purple
2109          <NA>              <NA>                    <NA>          purple
2110             3             2.395                   0.212          purple
2111             2             1.496                   0.243          purple
2112             1             1.372                   0.228          purple
2113             1              2.29                   0.293          purple
2114             2             1.666                   0.291          purple
2115          <NA>              <NA>                    <NA>          purple
2116             2             1.664                   0.224          purple
2117             3             1.835                    0.24          purple
2118             1             1.953                   0.227          purple
2119             3              1.45                   0.242          purple
2120             1             1.481                    <NA>           white
2121          <NA>              <NA>                    <NA>           white
2122             1              1.16                   0.178           white
2123             3             0.994                    <NA>           white
2124             2              <NA>                   0.151           white
2125             1             1.916                    0.18          purple
2126             2             2.355                   0.175          purple
2127             3             1.596                   0.203          purple
2128             2             2.051                   0.246          purple
2129             3             2.273                   0.166          purple
2130             2             2.161                   0.191          purple
2131             3             1.989                   0.181          purple
2132             1             2.172                   0.207          purple
2133             1             3.002                   0.286          purple
2134             2             3.175                    0.23          purple
2135             3             2.526                   0.207          purple
2136             1             2.704                   0.161          purple
2137             1             1.944                   0.173          purple
2138             1              1.92                   0.214          purple
2139             2             2.386                   0.238          purple
2140             2             1.904                   0.224          purple
2141             3             2.338                   0.235          purple
2142             3             2.398                   0.198          purple
2143             2             2.411                   0.248          purple
2144             1             2.192                   0.207          purple
2145          <NA>              <NA>                    <NA>          purple
2146             3             2.144                    0.19          purple
2147             2              2.05                   0.141          purple
2148             1             2.345                   0.244          purple
2149          <NA>              <NA>                    <NA>          purple
2150             1             3.488                   0.153          purple
2151             2             4.056                   0.244          purple
2152             3              5.85                   0.456          purple
2153             1             4.589                   0.266          purple
2154             3             3.312                   0.288          purple
2155             2             4.551                   0.258          purple
2156             3             1.498                   0.268          purple
2157             3             0.896                   0.157          purple
2158             1             0.953                   0.188          purple
2159          <NA>              <NA>                    <NA>          purple
2160             1             1.454                    0.22          purple
2161             2             1.221                   0.214          purple
2162             2              0.83                   0.168          purple
2163             2             1.941                   0.181          purple
2164             1             2.111                   0.197          purple
2165             2             2.639                   0.152          purple
2166          <NA>              <NA>                    <NA>          purple
2167             1              2.09                   0.228          purple
2168             3             2.573                   0.163          purple
2169             2             1.876                    0.16          purple
2170             2             1.794                   0.177          purple
2171             3             1.897                   0.195          purple
2172             3             2.609                   0.215          purple
2173             1             2.648                   0.173          purple
2174             3             1.866                   0.201          purple
2175             3             1.842                   0.226          purple
2176             3             1.916                    <NA>          purple
2177             1             2.458                   0.195          purple
2178             1             2.239                   0.182          purple
2179             2             2.139                    0.18          purple
2180             2             2.104                   0.187          purple
2181             2             1.456                    0.16          purple
2182             1             1.582                   0.201          purple
2183             1             2.004                   0.179          purple
2184             3             2.523                   0.234          purple
2185             1             3.219                   0.296          purple
2186             2             2.107                   0.233          purple
2187             3             2.776                   0.275          purple
2188             2             1.456                   0.214          purple
2189             2             3.152                    0.26          purple
2190             3             3.132                   0.273          purple
2191          <NA>              <NA>                    <NA>          purple
2192             1             1.704                   0.224          purple
2193             1              2.26                   0.249          purple
2194             2             2.146                   0.155          purple
2195             3             1.623                   0.258          purple
2196             1             2.662                    0.26          purple
2197             1             2.632                   0.228          purple
2198             2             2.559                   0.212          purple
2199             1             1.246                   0.191          purple
2200             3             2.701                   0.221          purple
2201             2             2.496                   0.219          purple
2202             3             2.735                   0.278          purple
2203             1             2.806                    0.25          purple
2204             1             3.483                   0.289          purple
2205             2             3.014                   0.311          purple
2206             1             3.334                   0.251          purple
2207             2             3.605                   0.253          purple
2208             3             2.341                   0.227          purple
2209          <NA>              <NA>                    <NA>          purple
2210             2             2.521                   0.223          purple
2211             2              2.55                   0.172          purple
2212             3             3.027                   0.295          purple
2213             3             2.649                   0.205          purple
2214             3             2.377                   0.278          purple
2215             2             3.009                   0.241          purple
2216             1             2.523                   0.209          purple
2217             1             3.179                   0.311          purple
2218          <NA>              <NA>                    <NA>          purple
2219             1             1.647                   0.175          purple
2220             3             1.588                    0.26          purple
2221             2             1.414                   0.313          purple
2222             3              1.29                   0.234          purple
2223          <NA>              <NA>                    <NA>          purple
2224             2             1.609                   0.197          purple
2225             1             1.588                   0.234          purple
2226             3              2.32                   0.276          purple
2227             2             2.122                   0.232          purple
2228             2             1.978                    0.31          purple
2229          <NA>              <NA>                    <NA>          purple
2230             1             2.406                   0.314          purple
2231             1             2.421                   0.233          purple
2232             3             2.468                   0.237          purple
2233             1             2.906                   0.261          purple
2234             3              2.58                   0.249          purple
2235             2             2.863                   0.273          purple
2236             2             2.647                   0.341          purple
2237             1              3.15                    0.36          purple
2238             1             2.555                   0.267          purple
2239             3             2.595                   0.268          purple
2240             2             2.202                   0.204          purple
2241             3             2.209                    0.28          purple
2242             3             2.014                   0.218          purple
2243             3              3.05                   0.305          purple
2244          <NA>              <NA>                    <NA>          purple
2245             2             2.275                    0.21          purple
2246             2             2.562                   0.275          purple
2247             1             2.697                   0.269          purple
2248             1              2.66                   0.246          purple
2249             1             1.553                    0.35          purple
2250             3             1.892                   0.211          purple
2251          <NA>              <NA>                    <NA>          purple
2252             2             1.634                    0.28          purple
2253             3             1.533                    <NA>          purple
2254             2             1.716                   0.472          purple
2255             1             1.972                   0.284          purple
2256             2             1.706                   0.276          purple
2257             3             2.584                    0.29          purple
2258             1             2.092                   0.285          purple
2259             2             0.915                   0.166           white
2260             1             1.108                   0.114           white
2261             3             0.975                   0.148           white
2262             3              1.03                   0.159           white
2263             2             0.943                   0.109           white
2264             1             1.019                    0.18           white
2265             2             1.022                   0.139           white
2266             1             0.723                   0.121           white
2267             3             0.988                   0.144           white
2268          <NA>              <NA>                    <NA>           white
2269             3             2.081                   0.188          purple
2270             2              2.88                   0.261          purple
2271             1              2.84                   0.199          purple
2272             2             2.335                   0.251          purple
2273             2             3.034                   0.233          purple
2274             3             3.023                   0.225          purple
2275             1             3.184                   0.234          purple
2276             3             3.105                   0.275          purple
2277             1             2.561                   0.237          purple
2278          <NA>              <NA>                    <NA>          purple
2279             3             2.351                   0.278          purple
2280             2             1.975                   0.257          purple
2281             1             2.241                   0.329          purple
2282          <NA>              <NA>                    <NA>          purple
2283             2             2.344                   0.228          purple
2284             1              2.66                    0.23          purple
2285             2             2.461                   0.217          purple
2286             3             2.448                   0.221          purple
2287             1             2.371                   0.217          purple
2288             3             2.056                   0.231          purple
2289             3             2.047                   0.159          purple
2290             1             2.649                   0.208          purple
2291             2             2.486                    0.24          purple
2292             2             2.174                   0.251          purple
2293             1             2.132                   0.247          purple
2294             3             2.224                   0.222          purple
2295          <NA>              <NA>                    <NA>          purple
2296             3             1.856                    <NA>          purple
2297             1             1.523                   0.359          purple
2298             1             1.105                   0.189          purple
2299             2             1.078                   0.202          purple
2300             3              1.39                    0.24          purple
2301             3             1.831                   0.198          purple
2302             2             2.116                    <NA>          purple
2303             1             1.483                   0.143          purple
2304             1             1.751                   0.182          purple
2305             3             1.085                   0.168          purple
2306             2              1.79                   0.238          purple
2307             2             1.607                   0.208          purple
2308             2             1.565                   0.213          purple
2309             1             1.599                   0.214          purple
2310             3             1.399                   0.161          purple
2311             1             1.709                   0.139           white
2312             2             1.897                   0.212           white
2313             3             1.524                    0.24           white
2314          <NA>              <NA>                    <NA>           white
2315             2             2.906                   0.215          purple
2316             2             1.952                    <NA>          purple
2317             3             2.013                    <NA>          purple
2318             3             2.331                    <NA>          purple
2319             1             2.077                   0.231          purple
2320             1             2.846                   0.218          purple
2321          <NA>              <NA>                    <NA>          purple
2322             3             3.001                    0.31          purple
2323             1             2.579                   0.307          purple
2324             2              3.08                   0.316          purple
2325             3             3.103                   0.326          purple
2326             1             3.541                   0.278          purple
2327             2             3.808                   0.296          purple
2328             1             3.965                   0.385          purple
2329             2             3.794                   0.324          purple
2330             3             3.351                   0.378          purple
2331             2             2.511                   0.287          purple
2332             3             3.128                   0.238          purple
2333             1             1.655                   0.232          purple
2334             2             1.411                    0.26          purple
2335          <NA>              <NA>                    <NA>          purple
2336             1             2.452                   0.308          purple
2337             2             3.021                     0.3          purple
2338             3             3.253                   0.211          purple
2339             3             1.732                   0.264          purple
2340             1             3.035                   0.224          purple
2341             2             1.642                    <NA>     blue-purple
2342             1             2.674                    <NA>     blue-purple
2343          <NA>              <NA>                    <NA>     blue-purple
2344             3              1.96                   0.299          purple
2345             1             1.643                   0.242          purple
2346             2             2.366                   0.232          purple
2347             1             2.241                   0.248          purple
2348             2             1.848                   0.233          purple
2349             3             1.714                    <NA>          purple
2350             1             2.282                   0.339          purple
2351             2             1.616                   0.201          purple
2352          <NA>              <NA>                    <NA>          purple
2353             3             1.899                   0.279          purple
2354             2             2.394                   0.202            pink
2355             3              2.08                    <NA>            pink
2356             3             2.191                   0.231            pink
2357             1             2.638                   0.285            pink
2358             1             2.246                   0.237            pink
2359             1               2.6                    <NA>            pink
2360             2             1.741                    <NA>            pink
2361          <NA>              <NA>                    <NA>            pink
2362             2             2.624                    <NA>            pink
2363             1              1.48                   0.264           white
2364             2             1.335                   0.229           white
2365             3             1.385                   0.195           white
2366             1             1.283                   0.247           white
2367             2             1.204                   0.202           white
2368             3             1.555                   0.232           white
2369             3             1.225                   0.221           white
2370             2             1.981                   0.209           white
2371             1             1.367                   0.261           white
2372          <NA>              <NA>                    <NA>           white
2373          <NA>              <NA>                    <NA>           white
2374             2             1.342                   0.199           white
2375             3             1.113                   0.222           white
2376             3             1.301                    <NA>           white
2377             1             1.486                   0.179           white
2378             2             1.326                   0.197           white
2379             1             1.143                   0.199           white
2380             1             2.207                   0.193          purple
2381             2             2.352                   0.155          purple
2382             3             2.146                   0.219          purple
2383             3             2.085                   0.203          purple
2384             1             2.078                   0.185          purple
2385             2             2.394                   0.172          purple
2386             3             1.984                   0.181          purple
2387             1             2.139                   0.162          purple
2388          <NA>              <NA>                    <NA>          purple
2389             2              2.44                   0.239          purple
2390             1             2.596                   0.283          purple
2391             3             2.512                   0.235          purple
2392             2              2.61                   0.228          purple
2393          <NA>              <NA>                    <NA>          purple
2394             1             1.945                   0.188          purple
2395             2              1.85                   0.217          purple
2396             3              1.58                   0.206          purple
2397             1              3.36                   0.256          purple
2398             2             2.821                   0.211          purple
2399             1             2.781                   0.204          purple
2400             2               3.2                   0.204          purple
2401          <NA>              <NA>                    <NA>          purple
2402             3             3.165                   0.308          purple
2403             1             3.369                    0.25          purple
2404             2             3.036                   0.271          purple
2405             2             3.871                   0.302          purple
2406             3             3.094                   0.283          purple
2407             1             2.407                   0.238          purple
2408             3             3.391                    <NA>          purple
2409             3             2.622                   0.234          purple
2410             1             3.879                   0.294          purple
2411             1              1.53                   0.162          purple
2412             3             1.749                   0.122          purple
2413             1             2.811                   0.277          purple
2414             2             1.865                    <NA>          purple
2415             2              1.13                   0.159          purple
2416             1             1.386                   0.211          purple
2417             2             1.364                   0.202          purple
2418             2             1.581                   0.167          purple
2419             3               1.2                    0.17          purple
2420             3             2.377                   0.255          purple
2421             3             1.427                   0.174          purple
2422          <NA>              <NA>                    <NA>          purple
2423             1             1.841                   0.164          purple
2424             2             1.286                    0.18          purple
2425             2             1.825                   0.212          purple
2426             3             1.575                   0.173          purple
2427             3             1.404                   0.221          purple
2428             1             1.608                   0.174          purple
2429             1             1.762                   0.214          purple
2430             1             1.785                   0.217          purple
2431             2             2.934                    0.23          purple
2432             2             1.619                   0.294           white
2433             1             1.657                   0.322           white
2434             3             1.377                   0.245           white
2435          <NA>              <NA>                    <NA>           white
2436             3             2.286                   0.175          purple
2437             2             2.052                    0.19          purple
2438             2             2.214                   0.273          purple
2439             3             2.268                   0.266          purple
2440          <NA>              <NA>                    <NA>          purple
2441             1             1.935                   0.286          purple
2442             1             1.967                   0.246          purple
2443             1             2.883                   0.346          purple
2444             2               2.6                   0.311          purple
2445             3             2.947                   0.291          purple
2446             2             1.959                   0.249          purple
2447             3             2.411                   0.205          purple
2448             3               2.3                    <NA>          purple
2449             1              1.67                   0.201          purple
2450             1             1.915                    <NA>          purple
2451             2             1.686                    <NA>          purple
2452          <NA>              <NA>                    <NA>          purple
2453             1             1.939                   0.235          purple
2454             2             1.733                   0.239          purple
2455             3             2.128                   0.197          purple
2456             1             2.271                   0.266          purple
2457             3             1.741                   0.143          purple
2458             2             2.571                   0.235          purple
2459             3             2.453                   0.237          purple
2460             2             1.721                   0.152          purple
2461             1             2.043                   0.136          purple
2462             3             2.831                   1.011          purple
2463          <NA>              <NA>                    <NA>          purple
2464             2             2.133                   0.942          purple
2465             3             2.251                   0.892          purple
2466             1             2.465                   0.688          purple
2467             1             2.093                   0.728          purple
2468             2             2.387                   1.053          purple
2469             1             2.326                   1.113          purple
2470             2             1.745                   0.482          purple
2471             3             1.719                   0.648          purple
2472             1              0.78                   0.425            blue
2473          <NA>              <NA>                    <NA>            blue
2474             2             0.738                   0.341            blue
2475             3             0.866                   0.284            blue
2476             2             1.638                   0.459            blue
2477          <NA>              <NA>                    <NA>            blue
2478             1             1.286                     0.5            blue
2479             1             0.971                   0.281            blue
2480             3             0.802                   0.198            blue
2481             1             1.078                   0.349            blue
2482             2             0.929                   0.528            blue
2483             3             0.964                   0.306            blue
2484             1             1.083                   0.372            blue
2485             2             1.013                   0.344            blue
2486             2             1.002                   0.299            blue
2487             3             0.787                   0.292            blue
2488          <NA>              <NA>                    <NA>            blue
2489             1             1.753                   0.689          purple
2490             2             1.875                   0.608          purple
2491          <NA>              <NA>                    <NA>          purple
2492             3             1.731                   0.527          purple
2493             1             1.276                   0.689          purple
2494             2             1.276                   0.727          purple
2495             3             1.646                   0.603          purple
2496             2             1.525                    <NA>          purple
2497             1             1.352                   0.483          purple
2498             2             1.224                   0.429          purple
2499             1             1.411                   0.356          purple
2500             3             1.533                   0.518          purple
2501          <NA>              <NA>                    <NA>          purple
2502             3             0.703                   0.199          purple
2503             1             0.679                   0.221          purple
2504             2             0.864                   0.241          purple
2505          <NA>              <NA>                    <NA>          purple
2506             3             1.948                    <NA>            blue
2507          <NA>              <NA>                    <NA>            blue
2508             1             2.255                   0.577            blue
2509             2             2.636                   0.579            blue
2510             1             3.459                   0.867            blue
2511             2              2.66                   0.524            blue
2512             3             3.046                    <NA>            blue
2513             3             2.104                   0.724            blue
2514             1               1.7                   0.613            blue
2515             2             2.194                   0.826            blue
2516             3             2.014                   0.812            blue
2517             1             2.359                   1.165            blue
2518             1             2.163                   0.849            blue
2519             2             2.043                   0.773            blue
2520             1             2.237                   0.886            blue
2521             2             1.988                   0.819            blue
2522             1             0.845                   0.304          purple
2523             1             0.811                   0.234          purple
2524             1             1.091                   0.437          purple
2525             2              1.18                   0.442          purple
2526             2             0.733                   0.446          purple
2527             3              <NA>                   0.502          purple
2528             2              0.97                   0.439          purple
2529             3             1.192                   0.584          purple
2530             3             1.017                   0.405          purple
2531          <NA>              <NA>                    <NA>          purple
2532             3             0.885                   0.266     blue-purple
2533             2             0.811                   0.248     blue-purple
2534             1              0.96                   0.434     blue-purple
2535             1              1.34                   0.384     blue-purple
2536             3             1.095                   0.381     blue-purple
2537             3             0.909                   0.363     blue-purple
2538             2             1.014                   0.322     blue-purple
2539             2             1.237                   0.282     blue-purple
2540             1             0.716                   0.311     blue-purple
2541          <NA>              <NA>                    <NA>     blue-purple
2542          <NA>              <NA>                    <NA>     blue-purple
2543             1             1.259                   0.262     blue-purple
2544             2             0.891                    0.21     blue-purple
2545             1             1.353                   0.393     blue-purple
2546             1             1.387                   0.472     blue-purple
2547             1             1.007                   0.366     blue-purple
2548             2             1.047                   0.297     blue-purple
2549             3             1.445                   0.483     blue-purple
2550             1             1.045                   0.449     blue-purple
2551             2              1.09                   0.443     blue-purple
2552          <NA>              <NA>                    <NA>     blue-purple
2553             3             1.007                   0.247     blue-purple
2554             1             1.419                   0.674     blue-purple
2555             2             1.641                   0.463     blue-purple
2556             2             1.109                   0.295     blue-purple
2557             2              1.18                   0.431     blue-purple
2558             3             1.242                   0.433     blue-purple
2559             1              1.16                    0.39     blue-purple
2560             3             1.209                   0.427     blue-purple
2561             3             1.196                   0.382     blue-purple
2562             1             1.313                   0.359     blue-purple
2563             2             1.265                   0.399     blue-purple
2564             1             1.165                   0.296     blue-purple
2565             2             1.209                    0.36     blue-purple
2566             3             1.279                   0.373     blue-purple
2567          <NA>              <NA>                    <NA>      light blue
2568             2             2.476                   1.474      light blue
2569             3             2.591                    <NA>      light blue
2570             1             2.699                   0.886      light blue
2571             1             2.934                   0.796      light blue
2572             1             2.578                    0.87      light blue
2573             2             2.558                   0.762      light blue
2574             2             1.022                   0.471            blue
2575             2             1.315                   0.504            blue
2576             1             1.154                   0.543            blue
2577          <NA>              <NA>                    <NA>            blue
2578             3             0.917                    0.47            blue
2579             3             1.389                   0.435            blue
2580             1             1.023                   0.557            blue
2581             1             1.011                   0.469            blue
2582             2             1.015                   0.478            blue
2583             3             1.004                   0.482            blue
2584             1             0.869                   0.246           white
2585             1             0.572                   0.302           white
2586             1             0.677                   0.165           white
2587             2             0.533                   0.165           white
2588             1             0.621                   0.266           white
2589             2              0.68                    0.17           white
2590             3             0.673                    0.15           white
2591          <NA>              <NA>                    <NA>           white
2592             1              1.18                   0.419            blue
2593             1             1.417                    0.33            blue
2594             3             1.131                    0.31            blue
2595             1             1.181                    0.54            blue
2596             2             1.248                   0.378            blue
2597             1             1.316                   0.402            blue
2598             2             1.351                    0.45            blue
2599             1             1.211                   0.489            blue
2600             2             1.492                   0.427            blue
2601             3              1.18                    <NA>            blue
2602          <NA>              <NA>                    <NA>            blue
2603             1              1.13                   0.474            blue
2604             2             0.991                   0.254            blue
2605             3             1.213                   0.318            blue
2606             1             0.973                   0.326            blue
2607             2             0.979                   0.409            blue
2608             1             3.036                   0.295          yellow
2609             1             2.594                   0.356          yellow
2610          <NA>              <NA>                    <NA>          yellow
2611             1             0.796                   0.257           white
2612          <NA>              <NA>                    <NA>           white
2613             2             2.375                   0.884            blue
2614             3             1.958                    0.72            blue
2615             1             1.575                   0.785            blue
2616             1             1.917                   0.824            blue
2617             3             2.027                   0.602            blue
2618          <NA>              <NA>                    <NA>            blue
2619             2             2.117                   0.588            blue
2620             1             2.285                   0.852            blue
2621             2             1.191                   0.328            blue
2622          <NA>              <NA>                    <NA>            blue
2623             3             1.926                   0.869            blue
2624             1             0.861                    0.33            blue
2625             1               1.2                   0.361            blue
2626             2             1.983                   0.922            blue
2627             1             1.197                   0.415            blue
2628             2             1.086                   0.384            blue
2629             1             1.397                   0.501            blue
2630             2             1.582                    0.59            blue
2631             3             1.153                   0.474            blue
2632             1             0.981                    0.39            blue
2633             2             0.877                   0.331            blue
2634             1             1.404                   0.633            blue
2635             2             1.064                    <NA>            blue
2636             3             1.171                   0.483            blue
2637             1             1.275                    0.62            blue
2638             2             1.368                   0.455            blue
2639             3             0.978                    <NA>            blue
2640          <NA>              <NA>                    <NA>            blue
2641             2              1.25                   0.436            blue
2642             3             1.216                    <NA>            blue
2643             3             1.037                   0.371            blue
2644             1             1.342                   0.482            blue
2645             3             1.539                   0.361            blue
2646             1             1.517                   0.437            blue
2647             2             1.544                   0.488            blue
2648             2              2.13                   0.486     blue-purple
2649             1             2.304                   0.595     blue-purple
2650             1             1.681                   0.482     blue-purple
2651             2             1.889                   0.729     blue-purple
2652             3              <NA>                   0.776     blue-purple
2653          <NA>              <NA>                    <NA>     blue-purple
2654             1             2.071                   0.591     blue-purple
2655             2             1.976                   0.675     blue-purple
2656             3             1.814                   0.718     blue-purple
2657             3             2.057                   0.304     blue-purple
2658             2             1.853                    0.39     blue-purple
2659             1             2.029                   0.317     blue-purple
2660          <NA>              <NA>                    <NA>           white
2661             3             0.891                   0.226           white
2662             3             1.018                   0.344           white
2663             2             0.869                    0.19           white
2664             3             0.688                   0.229           white
2665             1             0.774                   0.255           white
2666             2             0.741                   0.211           white
2667             1             0.757                   0.212           white
2668             2             0.945                   0.279           white
2669             1             0.714                    0.23           white
2670             1             0.857                    0.25           white
2671             2             1.087                   0.397           white
2672             3             0.926                    0.33           white
2673             2             0.718                   0.164           white
2674             3             0.717                   0.238           white
2675             1             0.713                   0.223           white
2676             1              1.15                   0.261          purple
2677             3             0.729                    0.16          purple
2678          <NA>              <NA>                    <NA>          purple
2679             2             0.991                    0.25          purple
2680             3             0.943                   0.323          purple
2681             1              1.23                   0.431          purple
2682             2             0.947                   0.211          purple
2683             3             0.965                   0.259          purple
2684             1               0.5                   0.186          purple
2685             2             0.739                   0.145          purple
2686          <NA>              <NA>                    <NA>            pink
2687             1             0.846                   0.235            pink
2688             2             0.792                   0.224            pink
2689             3             0.806                   0.177            pink
       color_opt_2 color_opt_3 color_opt_4
1             <NA>        <NA>        <NA>
2             <NA>        <NA>        <NA>
3             <NA>        <NA>        <NA>
4             <NA>        <NA>        <NA>
5             <NA>        <NA>        <NA>
6             <NA>        <NA>        <NA>
7             <NA>        <NA>        <NA>
8             <NA>        <NA>        <NA>
9             <NA>        <NA>        <NA>
10            <NA>        <NA>        <NA>
11            <NA>        <NA>        <NA>
12            <NA>        <NA>        <NA>
13            <NA>        <NA>        <NA>
14            <NA>        <NA>        <NA>
15            <NA>        <NA>        <NA>
16            <NA>        <NA>        <NA>
17            <NA>        <NA>        <NA>
18            <NA>        <NA>        <NA>
19            <NA>        <NA>        <NA>
20            <NA>        <NA>        <NA>
21            <NA>        <NA>        <NA>
22            <NA>        <NA>        <NA>
23            <NA>        <NA>        <NA>
24            <NA>        <NA>        <NA>
25            <NA>        <NA>        <NA>
26            <NA>        <NA>        <NA>
27            <NA>        <NA>        <NA>
28            <NA>        <NA>        <NA>
29            <NA>        <NA>        <NA>
30            <NA>        <NA>        <NA>
31            <NA>        <NA>        <NA>
32            <NA>        <NA>        <NA>
33            <NA>        <NA>        <NA>
34            <NA>        <NA>        <NA>
35            <NA>        <NA>        <NA>
36            <NA>        <NA>        <NA>
37            <NA>        <NA>        <NA>
38            <NA>        <NA>        <NA>
39            <NA>        <NA>        <NA>
40            <NA>        <NA>        <NA>
41            <NA>        <NA>        <NA>
42            <NA>        <NA>        <NA>
43            <NA>        <NA>        <NA>
44            <NA>        <NA>        <NA>
45            <NA>        <NA>        <NA>
46            <NA>        <NA>        <NA>
47            <NA>        <NA>        <NA>
48            <NA>        <NA>        <NA>
49            <NA>        <NA>        <NA>
50            <NA>        <NA>        <NA>
51            <NA>        <NA>        <NA>
52            <NA>        <NA>        <NA>
53            <NA>        <NA>        <NA>
54            <NA>        <NA>        <NA>
55            <NA>        <NA>        <NA>
56            <NA>        <NA>        <NA>
57            <NA>        <NA>        <NA>
58            <NA>        <NA>        <NA>
59            <NA>        <NA>        <NA>
60            <NA>        <NA>        <NA>
61            <NA>        <NA>        <NA>
62            <NA>        <NA>        <NA>
63            <NA>        <NA>        <NA>
64            <NA>        <NA>        <NA>
65            <NA>        <NA>        <NA>
66            <NA>        <NA>        <NA>
67            <NA>        <NA>        <NA>
68            <NA>        <NA>        <NA>
69            <NA>        <NA>        <NA>
70            <NA>        <NA>        <NA>
71            <NA>        <NA>        <NA>
72            <NA>        <NA>        <NA>
73            <NA>        <NA>        <NA>
74            <NA>        <NA>        <NA>
75            <NA>        <NA>        <NA>
76            <NA>        <NA>        <NA>
77            <NA>        <NA>        <NA>
78            <NA>        <NA>        <NA>
79            <NA>        <NA>        <NA>
80            <NA>        <NA>        <NA>
81            <NA>        <NA>        <NA>
82            <NA>        <NA>        <NA>
83            <NA>        <NA>        <NA>
84            <NA>        <NA>        <NA>
85            <NA>        <NA>        <NA>
86            <NA>        <NA>        <NA>
87            <NA>        <NA>        <NA>
88            <NA>        <NA>        <NA>
89            <NA>        <NA>        <NA>
90            <NA>        <NA>        <NA>
91            <NA>        <NA>        <NA>
92            <NA>        <NA>        <NA>
93            <NA>        <NA>        <NA>
94            <NA>        <NA>        <NA>
95            <NA>        <NA>        <NA>
96            <NA>        <NA>        <NA>
97            <NA>        <NA>        <NA>
98            <NA>        <NA>        <NA>
99            <NA>        <NA>        <NA>
100           <NA>        <NA>        <NA>
101           <NA>        <NA>        <NA>
102           <NA>        <NA>        <NA>
103           <NA>        <NA>        <NA>
104           <NA>        <NA>        <NA>
105           <NA>        <NA>        <NA>
106           <NA>        <NA>        <NA>
107           <NA>        <NA>        <NA>
108           <NA>        <NA>        <NA>
109           <NA>        <NA>        <NA>
110           <NA>        <NA>        <NA>
111           <NA>        <NA>        <NA>
112           <NA>        <NA>        <NA>
113           <NA>        <NA>        <NA>
114           <NA>        <NA>        <NA>
115           <NA>        <NA>        <NA>
116           <NA>        <NA>        <NA>
117           <NA>        <NA>        <NA>
118           <NA>        <NA>        <NA>
119           <NA>        <NA>        <NA>
120           <NA>        <NA>        <NA>
121           <NA>        <NA>        <NA>
122           <NA>        <NA>        <NA>
123           <NA>        <NA>        <NA>
124           <NA>        <NA>        <NA>
125           <NA>        <NA>        <NA>
126           <NA>        <NA>        <NA>
127           <NA>        <NA>        <NA>
128           <NA>        <NA>        <NA>
129           <NA>        <NA>        <NA>
130           <NA>        <NA>        <NA>
131           <NA>        <NA>        <NA>
132           <NA>        <NA>        <NA>
133           <NA>        <NA>        <NA>
134           <NA>        <NA>        <NA>
135           <NA>        <NA>        <NA>
136           <NA>        <NA>        <NA>
137           <NA>        <NA>        <NA>
138           <NA>        <NA>        <NA>
139           <NA>        <NA>        <NA>
140           <NA>        <NA>        <NA>
141           <NA>        <NA>        <NA>
142           <NA>        <NA>        <NA>
143           <NA>        <NA>        <NA>
144           <NA>        <NA>        <NA>
145           <NA>        <NA>        <NA>
146           <NA>        <NA>        <NA>
147           <NA>        <NA>        <NA>
148           <NA>        <NA>        <NA>
149           <NA>        <NA>        <NA>
150           <NA>        <NA>        <NA>
151           <NA>        <NA>        <NA>
152           <NA>        <NA>        <NA>
153           <NA>        <NA>        <NA>
154           <NA>        <NA>        <NA>
155           <NA>        <NA>        <NA>
156           <NA>        <NA>        <NA>
157           <NA>        <NA>        <NA>
158           <NA>        <NA>        <NA>
159           <NA>        <NA>        <NA>
160           <NA>        <NA>        <NA>
161           <NA>        <NA>        <NA>
162           <NA>        <NA>        <NA>
163           <NA>        <NA>        <NA>
164           <NA>        <NA>        <NA>
165           <NA>        <NA>        <NA>
166           <NA>        <NA>        <NA>
167           <NA>        <NA>        <NA>
168           <NA>        <NA>        <NA>
169           <NA>        <NA>        <NA>
170           <NA>        <NA>        <NA>
171           <NA>        <NA>        <NA>
172           <NA>        <NA>        <NA>
173           <NA>        <NA>        <NA>
174           <NA>        <NA>        <NA>
175           <NA>        <NA>        <NA>
176           <NA>        <NA>        <NA>
177           <NA>        <NA>        <NA>
178           <NA>        <NA>        <NA>
179           <NA>        <NA>        <NA>
180           <NA>        <NA>        <NA>
181           <NA>        <NA>        <NA>
182           <NA>        <NA>        <NA>
183           <NA>        <NA>        <NA>
184           <NA>        <NA>        <NA>
185           <NA>        <NA>        <NA>
186           <NA>        <NA>        <NA>
187           <NA>        <NA>        <NA>
188           <NA>        <NA>        <NA>
189           <NA>        <NA>        <NA>
190           <NA>        <NA>        <NA>
191           <NA>        <NA>        <NA>
192           <NA>        <NA>        <NA>
193           <NA>        <NA>        <NA>
194           <NA>        <NA>        <NA>
195           <NA>        <NA>        <NA>
196           <NA>        <NA>        <NA>
197           <NA>        <NA>        <NA>
198           <NA>        <NA>        <NA>
199           <NA>        <NA>        <NA>
200           <NA>        <NA>        <NA>
201           <NA>        <NA>        <NA>
202           <NA>        <NA>        <NA>
203           <NA>        <NA>        <NA>
204           <NA>        <NA>        <NA>
205           <NA>        <NA>        <NA>
206           <NA>        <NA>        <NA>
207           <NA>        <NA>        <NA>
208           <NA>        <NA>        <NA>
209           <NA>        <NA>        <NA>
210           <NA>        <NA>        <NA>
211           <NA>        <NA>        <NA>
212           <NA>        <NA>        <NA>
213           <NA>        <NA>        <NA>
214           <NA>        <NA>        <NA>
215           <NA>        <NA>        <NA>
216           <NA>        <NA>        <NA>
217           <NA>        <NA>        <NA>
218           <NA>        <NA>        <NA>
219           <NA>        <NA>        <NA>
220           <NA>        <NA>        <NA>
221           <NA>        <NA>        <NA>
222           <NA>        <NA>        <NA>
223           <NA>        <NA>        <NA>
224           <NA>        <NA>        <NA>
225           <NA>        <NA>        <NA>
226           <NA>        <NA>        <NA>
227           <NA>        <NA>        <NA>
228           <NA>        <NA>        <NA>
229           <NA>        <NA>        <NA>
230           <NA>        <NA>        <NA>
231           <NA>        <NA>        <NA>
232           <NA>        <NA>        <NA>
233           <NA>        <NA>        <NA>
234           <NA>        <NA>        <NA>
235           <NA>        <NA>        <NA>
236           <NA>        <NA>        <NA>
237           <NA>        <NA>        <NA>
238           <NA>        <NA>        <NA>
239           <NA>        <NA>        <NA>
240           <NA>        <NA>        <NA>
241           <NA>        <NA>        <NA>
242           <NA>        <NA>        <NA>
243           <NA>        <NA>        <NA>
244           <NA>        <NA>        <NA>
245           <NA>        <NA>        <NA>
246           <NA>        <NA>        <NA>
247           <NA>        <NA>        <NA>
248           <NA>        <NA>        <NA>
249           <NA>        <NA>        <NA>
250           <NA>        <NA>        <NA>
251           <NA>        <NA>        <NA>
252           <NA>        <NA>        <NA>
253           <NA>        <NA>        <NA>
254           <NA>        <NA>        <NA>
255           <NA>        <NA>        <NA>
256           <NA>        <NA>        <NA>
257           <NA>        <NA>        <NA>
258           <NA>        <NA>        <NA>
259           <NA>        <NA>        <NA>
260           <NA>        <NA>        <NA>
261           <NA>        <NA>        <NA>
262           <NA>        <NA>        <NA>
263           <NA>        <NA>        <NA>
264           <NA>        <NA>        <NA>
265           <NA>        <NA>        <NA>
266           <NA>        <NA>        <NA>
267           <NA>        <NA>        <NA>
268           <NA>        <NA>        <NA>
269           <NA>        <NA>        <NA>
270           <NA>        <NA>        <NA>
271           <NA>        <NA>        <NA>
272           <NA>        <NA>        <NA>
273           <NA>        <NA>        <NA>
274           <NA>        <NA>        <NA>
275           <NA>        <NA>        <NA>
276           <NA>        <NA>        <NA>
277           <NA>        <NA>        <NA>
278           <NA>        <NA>        <NA>
279           <NA>        <NA>        <NA>
280           <NA>        <NA>        <NA>
281           <NA>        <NA>        <NA>
282           <NA>        <NA>        <NA>
283           <NA>        <NA>        <NA>
284           <NA>        <NA>        <NA>
285           <NA>        <NA>        <NA>
286           <NA>        <NA>        <NA>
287           <NA>        <NA>        <NA>
288           <NA>        <NA>        <NA>
289           <NA>        <NA>        <NA>
290           <NA>        <NA>        <NA>
291           <NA>        <NA>        <NA>
292           <NA>        <NA>        <NA>
293           <NA>        <NA>        <NA>
294           <NA>        <NA>        <NA>
295           <NA>        <NA>        <NA>
296           <NA>        <NA>        <NA>
297           <NA>        <NA>        <NA>
298           <NA>        <NA>        <NA>
299           <NA>        <NA>        <NA>
300           <NA>        <NA>        <NA>
301           <NA>        <NA>        <NA>
302           <NA>        <NA>        <NA>
303           <NA>        <NA>        <NA>
304           <NA>        <NA>        <NA>
305           <NA>        <NA>        <NA>
306           <NA>        <NA>        <NA>
307           <NA>        <NA>        <NA>
308           <NA>        <NA>        <NA>
309           <NA>        <NA>        <NA>
310           <NA>        <NA>        <NA>
311           <NA>        <NA>        <NA>
312           <NA>        <NA>        <NA>
313           <NA>        <NA>        <NA>
314           <NA>        <NA>        <NA>
315           <NA>        <NA>        <NA>
316           <NA>        <NA>        <NA>
317           <NA>        <NA>        <NA>
318           <NA>        <NA>        <NA>
319           <NA>        <NA>        <NA>
320           <NA>        <NA>        <NA>
321           <NA>        <NA>        <NA>
322           <NA>        <NA>        <NA>
323           <NA>        <NA>        <NA>
324           <NA>        <NA>        <NA>
325           <NA>        <NA>        <NA>
326           <NA>        <NA>        <NA>
327           <NA>        <NA>        <NA>
328           <NA>        <NA>        <NA>
329           <NA>        <NA>        <NA>
330           <NA>        <NA>        <NA>
331           <NA>        <NA>        <NA>
332           <NA>        <NA>        <NA>
333           <NA>        <NA>        <NA>
334           <NA>        <NA>        <NA>
335           <NA>        <NA>        <NA>
336           <NA>        <NA>        <NA>
337           <NA>        <NA>        <NA>
338           <NA>        <NA>        <NA>
339           <NA>        <NA>        <NA>
340           <NA>        <NA>        <NA>
341           <NA>        <NA>        <NA>
342           <NA>        <NA>        <NA>
343           <NA>        <NA>        <NA>
344           <NA>        <NA>        <NA>
345           <NA>        <NA>        <NA>
346           <NA>        <NA>        <NA>
347           <NA>        <NA>        <NA>
348           <NA>        <NA>        <NA>
349           <NA>        <NA>        <NA>
350           <NA>        <NA>        <NA>
351           <NA>        <NA>        <NA>
352           <NA>        <NA>        <NA>
353           <NA>        <NA>        <NA>
354           <NA>        <NA>        <NA>
355           <NA>        <NA>        <NA>
356           <NA>        <NA>        <NA>
357           <NA>        <NA>        <NA>
358           <NA>        <NA>        <NA>
359           <NA>        <NA>        <NA>
360           <NA>        <NA>        <NA>
361           <NA>        <NA>        <NA>
362           <NA>        <NA>        <NA>
363           <NA>        <NA>        <NA>
364           <NA>        <NA>        <NA>
365           <NA>        <NA>        <NA>
366           <NA>        <NA>        <NA>
367           <NA>        <NA>        <NA>
368           <NA>        <NA>        <NA>
369           <NA>        <NA>        <NA>
370           <NA>        <NA>        <NA>
371           <NA>        <NA>        <NA>
372           <NA>        <NA>        <NA>
373           <NA>        <NA>        <NA>
374           <NA>        <NA>        <NA>
375           <NA>        <NA>        <NA>
376           <NA>        <NA>        <NA>
377           <NA>        <NA>        <NA>
378           <NA>        <NA>        <NA>
379           <NA>        <NA>        <NA>
380           <NA>        <NA>        <NA>
381           <NA>        <NA>        <NA>
382           <NA>        <NA>        <NA>
383           <NA>        <NA>        <NA>
384           <NA>        <NA>        <NA>
385           <NA>        <NA>        <NA>
386           <NA>        <NA>        <NA>
387           <NA>        <NA>        <NA>
388           <NA>        <NA>        <NA>
389           <NA>        <NA>        <NA>
390           <NA>        <NA>        <NA>
391           <NA>        <NA>        <NA>
392           <NA>        <NA>        <NA>
393           <NA>        <NA>        <NA>
394           <NA>        <NA>        <NA>
395           <NA>        <NA>        <NA>
396           <NA>        <NA>        <NA>
397           <NA>        <NA>        <NA>
398           <NA>        <NA>        <NA>
399           <NA>        <NA>        <NA>
400           <NA>        <NA>        <NA>
401           <NA>        <NA>        <NA>
402           <NA>        <NA>        <NA>
403           <NA>        <NA>        <NA>
404           <NA>        <NA>        <NA>
405           <NA>        <NA>        <NA>
406           <NA>        <NA>        <NA>
407           <NA>        <NA>        <NA>
408           <NA>        <NA>        <NA>
409           <NA>        <NA>        <NA>
410           <NA>        <NA>        <NA>
411           <NA>        <NA>        <NA>
412           <NA>        <NA>        <NA>
413           <NA>        <NA>        <NA>
414           <NA>        <NA>        <NA>
415           <NA>        <NA>        <NA>
416           <NA>        <NA>        <NA>
417           <NA>        <NA>        <NA>
418           <NA>        <NA>        <NA>
419           <NA>        <NA>        <NA>
420           <NA>        <NA>        <NA>
421           <NA>        <NA>        <NA>
422           <NA>        <NA>        <NA>
423           <NA>        <NA>        <NA>
424           <NA>        <NA>        <NA>
425           <NA>        <NA>        <NA>
426           <NA>        <NA>        <NA>
427           <NA>        <NA>        <NA>
428           <NA>        <NA>        <NA>
429           <NA>        <NA>        <NA>
430           <NA>        <NA>        <NA>
431           <NA>        <NA>        <NA>
432           <NA>        <NA>        <NA>
433           <NA>        <NA>        <NA>
434           <NA>        <NA>        <NA>
435           <NA>        <NA>        <NA>
436           <NA>        <NA>        <NA>
437           <NA>        <NA>        <NA>
438           <NA>        <NA>        <NA>
439           <NA>        <NA>        <NA>
440           <NA>        <NA>        <NA>
441           <NA>        <NA>        <NA>
442           <NA>        <NA>        <NA>
443           <NA>        <NA>        <NA>
444           <NA>        <NA>        <NA>
445           <NA>        <NA>        <NA>
446           <NA>        <NA>        <NA>
447           <NA>        <NA>        <NA>
448           <NA>        <NA>        <NA>
449           <NA>        <NA>        <NA>
450           <NA>        <NA>        <NA>
451           <NA>        <NA>        <NA>
452           <NA>        <NA>        <NA>
453           <NA>        <NA>        <NA>
454           <NA>        <NA>        <NA>
455           <NA>        <NA>        <NA>
456           <NA>        <NA>        <NA>
457           <NA>        <NA>        <NA>
458           <NA>        <NA>        <NA>
459           <NA>        <NA>        <NA>
460           <NA>        <NA>        <NA>
461           <NA>        <NA>        <NA>
462           <NA>        <NA>        <NA>
463           <NA>        <NA>        <NA>
464           <NA>        <NA>        <NA>
465           <NA>        <NA>        <NA>
466           <NA>        <NA>        <NA>
467           <NA>        <NA>        <NA>
468           <NA>        <NA>        <NA>
469           <NA>        <NA>        <NA>
470           <NA>        <NA>        <NA>
471           <NA>        <NA>        <NA>
472           <NA>        <NA>        <NA>
473           <NA>        <NA>        <NA>
474           <NA>        <NA>        <NA>
475           <NA>        <NA>        <NA>
476           <NA>        <NA>        <NA>
477           <NA>        <NA>        <NA>
478           <NA>        <NA>        <NA>
479           <NA>        <NA>        <NA>
480           <NA>        <NA>        <NA>
481           <NA>        <NA>        <NA>
482           <NA>        <NA>        <NA>
483           <NA>        <NA>        <NA>
484           <NA>        <NA>        <NA>
485           <NA>        <NA>        <NA>
486           <NA>        <NA>        <NA>
487           <NA>        <NA>        <NA>
488           <NA>        <NA>        <NA>
489           <NA>        <NA>        <NA>
490           <NA>        <NA>        <NA>
491           <NA>        <NA>        <NA>
492           <NA>        <NA>        <NA>
493           <NA>        <NA>        <NA>
494           <NA>        <NA>        <NA>
495           <NA>        <NA>        <NA>
496           <NA>        <NA>        <NA>
497           <NA>        <NA>        <NA>
498           <NA>        <NA>        <NA>
499           <NA>        <NA>        <NA>
500           <NA>        <NA>        <NA>
501           <NA>        <NA>        <NA>
502           <NA>        <NA>        <NA>
503           <NA>        <NA>        <NA>
504           <NA>        <NA>        <NA>
505           <NA>        <NA>        <NA>
506           <NA>        <NA>        <NA>
507           <NA>        <NA>        <NA>
508           <NA>        <NA>        <NA>
509           <NA>        <NA>        <NA>
510           <NA>        <NA>        <NA>
511           <NA>        <NA>        <NA>
512           <NA>        <NA>        <NA>
513           <NA>        <NA>        <NA>
514           <NA>        <NA>        <NA>
515           <NA>        <NA>        <NA>
516           <NA>        <NA>        <NA>
517           <NA>        <NA>        <NA>
518           <NA>        <NA>        <NA>
519           <NA>        <NA>        <NA>
520           <NA>        <NA>        <NA>
521           <NA>        <NA>        <NA>
522           <NA>        <NA>        <NA>
523           <NA>        <NA>        <NA>
524           <NA>        <NA>        <NA>
525           <NA>        <NA>        <NA>
526           <NA>        <NA>        <NA>
527           <NA>        <NA>        <NA>
528           <NA>        <NA>        <NA>
529           <NA>        <NA>        <NA>
530           <NA>        <NA>        <NA>
531           <NA>        <NA>        <NA>
532           <NA>        <NA>        <NA>
533           <NA>        <NA>        <NA>
534           <NA>        <NA>        <NA>
535           <NA>        <NA>        <NA>
536           <NA>        <NA>        <NA>
537           <NA>        <NA>        <NA>
538           <NA>        <NA>        <NA>
539           <NA>        <NA>        <NA>
540           <NA>        <NA>        <NA>
541           <NA>        <NA>        <NA>
542           <NA>        <NA>        <NA>
543           <NA>        <NA>        <NA>
544           <NA>        <NA>        <NA>
545           <NA>        <NA>        <NA>
546           <NA>        <NA>        <NA>
547           <NA>        <NA>        <NA>
548           <NA>        <NA>        <NA>
549           <NA>        <NA>        <NA>
550           <NA>        <NA>        <NA>
551           <NA>        <NA>        <NA>
552           <NA>        <NA>        <NA>
553           <NA>        <NA>        <NA>
554           <NA>        <NA>        <NA>
555           <NA>        <NA>        <NA>
556           <NA>        <NA>        <NA>
557           <NA>        <NA>        <NA>
558           <NA>        <NA>        <NA>
559           <NA>        <NA>        <NA>
560           <NA>        <NA>        <NA>
561           <NA>        <NA>        <NA>
562           <NA>        <NA>        <NA>
563           <NA>        <NA>        <NA>
564           <NA>        <NA>        <NA>
565           <NA>        <NA>        <NA>
566           <NA>        <NA>        <NA>
567           <NA>        <NA>        <NA>
568           <NA>        <NA>        <NA>
569           <NA>        <NA>        <NA>
570           <NA>        <NA>        <NA>
571           <NA>        <NA>        <NA>
572           <NA>        <NA>        <NA>
573           <NA>        <NA>        <NA>
574           <NA>        <NA>        <NA>
575           <NA>        <NA>        <NA>
576           <NA>        <NA>        <NA>
577           <NA>        <NA>        <NA>
578           <NA>        <NA>        <NA>
579           <NA>        <NA>        <NA>
580           <NA>        <NA>        <NA>
581           <NA>        <NA>        <NA>
582           <NA>        <NA>        <NA>
583           <NA>        <NA>        <NA>
584           <NA>        <NA>        <NA>
585           <NA>        <NA>        <NA>
586           <NA>        <NA>        <NA>
587           <NA>        <NA>        <NA>
588           <NA>        <NA>        <NA>
589           <NA>        <NA>        <NA>
590           <NA>        <NA>        <NA>
591           <NA>        <NA>        <NA>
592           <NA>        <NA>        <NA>
593           <NA>        <NA>        <NA>
594           <NA>        <NA>        <NA>
595           <NA>        <NA>        <NA>
596           <NA>        <NA>        <NA>
597           <NA>        <NA>        <NA>
598           <NA>        <NA>        <NA>
599           <NA>        <NA>        <NA>
600           <NA>        <NA>        <NA>
601           <NA>        <NA>        <NA>
602           <NA>        <NA>        <NA>
603           <NA>        <NA>        <NA>
604           <NA>        <NA>        <NA>
605           <NA>        <NA>        <NA>
606           <NA>        <NA>        <NA>
607           <NA>        <NA>        <NA>
608           <NA>        <NA>        <NA>
609           <NA>        <NA>        <NA>
610           <NA>        <NA>        <NA>
611           <NA>        <NA>        <NA>
612           <NA>        <NA>        <NA>
613           <NA>        <NA>        <NA>
614           <NA>        <NA>        <NA>
615           <NA>        <NA>        <NA>
616           <NA>        <NA>        <NA>
617           <NA>        <NA>        <NA>
618           <NA>        <NA>        <NA>
619           <NA>        <NA>        <NA>
620           <NA>        <NA>        <NA>
621           <NA>        <NA>        <NA>
622           <NA>        <NA>        <NA>
623           <NA>        <NA>        <NA>
624           <NA>        <NA>        <NA>
625           <NA>        <NA>        <NA>
626           <NA>        <NA>        <NA>
627           <NA>        <NA>        <NA>
628           <NA>        <NA>        <NA>
629           <NA>        <NA>        <NA>
630           <NA>        <NA>        <NA>
631           <NA>        <NA>        <NA>
632           <NA>        <NA>        <NA>
633           <NA>        <NA>        <NA>
634           <NA>        <NA>        <NA>
635           <NA>        <NA>        <NA>
636           <NA>        <NA>        <NA>
637           <NA>        <NA>        <NA>
638           <NA>        <NA>        <NA>
639           <NA>        <NA>        <NA>
640           <NA>        <NA>        <NA>
641           <NA>        <NA>        <NA>
642           <NA>        <NA>        <NA>
643           <NA>        <NA>        <NA>
644           <NA>        <NA>        <NA>
645           <NA>        <NA>        <NA>
646           <NA>        <NA>        <NA>
647           <NA>        <NA>        <NA>
648           <NA>        <NA>        <NA>
649           <NA>        <NA>        <NA>
650           <NA>        <NA>        <NA>
651           <NA>        <NA>        <NA>
652           <NA>        <NA>        <NA>
653           <NA>        <NA>        <NA>
654           <NA>        <NA>        <NA>
655           <NA>        <NA>        <NA>
656           <NA>        <NA>        <NA>
657           <NA>        <NA>        <NA>
658           <NA>        <NA>        <NA>
659           <NA>        <NA>        <NA>
660           <NA>        <NA>        <NA>
661           <NA>        <NA>        <NA>
662           <NA>        <NA>        <NA>
663           <NA>        <NA>        <NA>
664           <NA>        <NA>        <NA>
665           <NA>        <NA>        <NA>
666           <NA>        <NA>        <NA>
667           <NA>        <NA>        <NA>
668           <NA>        <NA>        <NA>
669           <NA>        <NA>        <NA>
670           <NA>        <NA>        <NA>
671           <NA>        <NA>        <NA>
672           <NA>        <NA>        <NA>
673           <NA>        <NA>        <NA>
674           <NA>        <NA>        <NA>
675           <NA>        <NA>        <NA>
676           <NA>        <NA>        <NA>
677           <NA>        <NA>        <NA>
678           <NA>        <NA>        <NA>
679           <NA>        <NA>        <NA>
680           <NA>        <NA>        <NA>
681           <NA>        <NA>        <NA>
682           <NA>        <NA>        <NA>
683           <NA>        <NA>        <NA>
684           <NA>        <NA>        <NA>
685           <NA>        <NA>        <NA>
686           <NA>        <NA>        <NA>
687           <NA>        <NA>        <NA>
688           <NA>        <NA>        <NA>
689           <NA>        <NA>        <NA>
690           <NA>        <NA>        <NA>
691           <NA>        <NA>        <NA>
692           <NA>        <NA>        <NA>
693           <NA>        <NA>        <NA>
694           <NA>        <NA>        <NA>
695           <NA>        <NA>        <NA>
696           <NA>        <NA>        <NA>
697           <NA>        <NA>        <NA>
698           <NA>        <NA>        <NA>
699           <NA>        <NA>        <NA>
700           <NA>        <NA>        <NA>
701           <NA>        <NA>        <NA>
702           <NA>        <NA>        <NA>
703           <NA>        <NA>        <NA>
704           <NA>        <NA>        <NA>
705           <NA>        <NA>        <NA>
706           <NA>        <NA>        <NA>
707           <NA>        <NA>        <NA>
708           <NA>        <NA>        <NA>
709           <NA>        <NA>        <NA>
710           <NA>        <NA>        <NA>
711           <NA>        <NA>        <NA>
712           <NA>        <NA>        <NA>
713           <NA>        <NA>        <NA>
714           <NA>        <NA>        <NA>
715           <NA>        <NA>        <NA>
716           <NA>        <NA>        <NA>
717           <NA>        <NA>        <NA>
718           <NA>        <NA>        <NA>
719           <NA>        <NA>        <NA>
720           <NA>        <NA>        <NA>
721           <NA>        <NA>        <NA>
722           <NA>        <NA>        <NA>
723           <NA>        <NA>        <NA>
724           <NA>        <NA>        <NA>
725           <NA>        <NA>        <NA>
726           <NA>        <NA>        <NA>
727           <NA>        <NA>        <NA>
728           <NA>        <NA>        <NA>
729           <NA>        <NA>        <NA>
730           <NA>        <NA>        <NA>
731           <NA>        <NA>        <NA>
732           <NA>        <NA>        <NA>
733           <NA>        <NA>        <NA>
734           <NA>        <NA>        <NA>
735           <NA>        <NA>        <NA>
736           <NA>        <NA>        <NA>
737           <NA>        <NA>        <NA>
738           <NA>        <NA>        <NA>
739           <NA>        <NA>        <NA>
740           <NA>        <NA>        <NA>
741           <NA>        <NA>        <NA>
742           <NA>        <NA>        <NA>
743           <NA>        <NA>        <NA>
744           <NA>        <NA>        <NA>
745           <NA>        <NA>        <NA>
746           <NA>        <NA>        <NA>
747           <NA>        <NA>        <NA>
748           <NA>        <NA>        <NA>
749           <NA>        <NA>        <NA>
750           <NA>        <NA>        <NA>
751           <NA>        <NA>        <NA>
752           <NA>        <NA>        <NA>
753           <NA>        <NA>        <NA>
754           <NA>        <NA>        <NA>
755           <NA>        <NA>        <NA>
756           <NA>        <NA>        <NA>
757           <NA>        <NA>        <NA>
758           <NA>        <NA>        <NA>
759           <NA>        <NA>        <NA>
760           <NA>        <NA>        <NA>
761           <NA>        <NA>        <NA>
762           <NA>        <NA>        <NA>
763           <NA>        <NA>        <NA>
764           <NA>        <NA>        <NA>
765           <NA>        <NA>        <NA>
766           <NA>        <NA>        <NA>
767           <NA>        <NA>        <NA>
768           <NA>        <NA>        <NA>
769           <NA>        <NA>        <NA>
770           <NA>        <NA>        <NA>
771           <NA>        <NA>        <NA>
772           <NA>        <NA>        <NA>
773           <NA>        <NA>        <NA>
774           <NA>        <NA>        <NA>
775           <NA>        <NA>        <NA>
776           <NA>        <NA>        <NA>
777           <NA>        <NA>        <NA>
778           <NA>        <NA>        <NA>
779           <NA>        <NA>        <NA>
780           <NA>        <NA>        <NA>
781           <NA>        <NA>        <NA>
782           <NA>        <NA>        <NA>
783           <NA>        <NA>        <NA>
784           <NA>        <NA>        <NA>
785           <NA>        <NA>        <NA>
786           <NA>        <NA>        <NA>
787           <NA>        <NA>        <NA>
788           <NA>        <NA>        <NA>
789           <NA>        <NA>        <NA>
790           <NA>        <NA>        <NA>
791           <NA>        <NA>        <NA>
792           <NA>        <NA>        <NA>
793           <NA>        <NA>        <NA>
794           <NA>        <NA>        <NA>
795           <NA>        <NA>        <NA>
796           <NA>        <NA>        <NA>
797           <NA>        <NA>        <NA>
798           <NA>        <NA>        <NA>
799           <NA>        <NA>        <NA>
800           <NA>        <NA>        <NA>
801           <NA>        <NA>        <NA>
802           <NA>        <NA>        <NA>
803           <NA>        <NA>        <NA>
804           <NA>        <NA>        <NA>
805           <NA>        <NA>        <NA>
806           <NA>        <NA>        <NA>
807           <NA>        <NA>        <NA>
808           <NA>        <NA>        <NA>
809           <NA>        <NA>        <NA>
810           <NA>        <NA>        <NA>
811           <NA>        <NA>        <NA>
812           <NA>        <NA>        <NA>
813           <NA>        <NA>        <NA>
814           <NA>        <NA>        <NA>
815           <NA>        <NA>        <NA>
816           <NA>        <NA>        <NA>
817           <NA>        <NA>        <NA>
818           <NA>        <NA>        <NA>
819           <NA>        <NA>        <NA>
820           <NA>        <NA>        <NA>
821           <NA>        <NA>        <NA>
822           <NA>        <NA>        <NA>
823           <NA>        <NA>        <NA>
824           <NA>        <NA>        <NA>
825           <NA>        <NA>        <NA>
826           <NA>        <NA>        <NA>
827           <NA>        <NA>        <NA>
828           <NA>        <NA>        <NA>
829           <NA>        <NA>        <NA>
830           <NA>        <NA>        <NA>
831           <NA>        <NA>        <NA>
832           <NA>        <NA>        <NA>
833           <NA>        <NA>        <NA>
834           <NA>        <NA>        <NA>
835           <NA>        <NA>        <NA>
836           <NA>        <NA>        <NA>
837           <NA>        <NA>        <NA>
838           <NA>        <NA>        <NA>
839           <NA>        <NA>        <NA>
840           <NA>        <NA>        <NA>
841           <NA>        <NA>        <NA>
842           <NA>        <NA>        <NA>
843           <NA>        <NA>        <NA>
844           <NA>        <NA>        <NA>
845           <NA>        <NA>        <NA>
846           <NA>        <NA>        <NA>
847           <NA>        <NA>        <NA>
848           <NA>        <NA>        <NA>
849           <NA>        <NA>        <NA>
850           <NA>        <NA>        <NA>
851           <NA>        <NA>        <NA>
852           <NA>        <NA>        <NA>
853           <NA>        <NA>        <NA>
854           <NA>        <NA>        <NA>
855           <NA>        <NA>        <NA>
856           <NA>        <NA>        <NA>
857           <NA>        <NA>        <NA>
858           <NA>        <NA>        <NA>
859           <NA>        <NA>        <NA>
860           <NA>        <NA>        <NA>
861           <NA>        <NA>        <NA>
862           <NA>        <NA>        <NA>
863           <NA>        <NA>        <NA>
864           <NA>        <NA>        <NA>
865           <NA>        <NA>        <NA>
866           <NA>        <NA>        <NA>
867           <NA>        <NA>        <NA>
868           <NA>        <NA>        <NA>
869           <NA>        <NA>        <NA>
870           <NA>        <NA>        <NA>
871           <NA>        <NA>        <NA>
872           <NA>        <NA>        <NA>
873           <NA>        <NA>        <NA>
874           <NA>        <NA>        <NA>
875           <NA>        <NA>        <NA>
876           <NA>        <NA>        <NA>
877           <NA>        <NA>        <NA>
878           <NA>        <NA>        <NA>
879           <NA>        <NA>        <NA>
880           <NA>        <NA>        <NA>
881         purple        <NA>        <NA>
882         purple        <NA>        <NA>
883         purple        <NA>        <NA>
884         purple        <NA>        <NA>
885         purple        <NA>        <NA>
886         purple        <NA>        <NA>
887         purple        <NA>        <NA>
888         purple        <NA>        <NA>
889         purple        <NA>        <NA>
890         purple        <NA>        <NA>
891         purple        <NA>        <NA>
892         purple        <NA>        <NA>
893         purple        <NA>        <NA>
894         purple        <NA>        <NA>
895         purple        <NA>        <NA>
896         purple        <NA>        <NA>
897           <NA>        <NA>        <NA>
898           <NA>        <NA>        <NA>
899           <NA>        <NA>        <NA>
900           <NA>        <NA>        <NA>
901           <NA>        <NA>        <NA>
902           <NA>        <NA>        <NA>
903           <NA>        <NA>        <NA>
904           <NA>        <NA>        <NA>
905           <NA>        <NA>        <NA>
906           <NA>        <NA>        <NA>
907           <NA>        <NA>        <NA>
908           <NA>        <NA>        <NA>
909           <NA>        <NA>        <NA>
910         purple        <NA>        <NA>
911         purple        <NA>        <NA>
912         purple        <NA>        <NA>
913         purple        <NA>        <NA>
914         purple        <NA>        <NA>
915         purple        <NA>        <NA>
916         purple        <NA>        <NA>
917         purple        <NA>        <NA>
918         purple        <NA>        <NA>
919         purple        <NA>        <NA>
920         purple        <NA>        <NA>
921         purple        <NA>        <NA>
922         purple        <NA>        <NA>
923         purple        <NA>        <NA>
924         purple        <NA>        <NA>
925         purple        <NA>        <NA>
926         purple        <NA>        <NA>
927         purple        <NA>        <NA>
928         purple        <NA>        <NA>
929         purple        <NA>        <NA>
930         purple        <NA>        <NA>
931         purple        <NA>        <NA>
932         purple        <NA>        <NA>
933           <NA>        <NA>        <NA>
934           <NA>        <NA>        <NA>
935           <NA>        <NA>        <NA>
936           <NA>        <NA>        <NA>
937           <NA>        <NA>        <NA>
938           <NA>        <NA>        <NA>
939           <NA>        <NA>        <NA>
940           <NA>        <NA>        <NA>
941           <NA>        <NA>        <NA>
942           <NA>        <NA>        <NA>
943           <NA>        <NA>        <NA>
944           <NA>        <NA>        <NA>
945           <NA>        <NA>        <NA>
946           <NA>        <NA>        <NA>
947           <NA>        <NA>        <NA>
948           <NA>        <NA>        <NA>
949           <NA>        <NA>        <NA>
950           <NA>        <NA>        <NA>
951           <NA>        <NA>        <NA>
952           <NA>        <NA>        <NA>
953           <NA>        <NA>        <NA>
954           <NA>        <NA>        <NA>
955           <NA>        <NA>        <NA>
956           <NA>        <NA>        <NA>
957           <NA>        <NA>        <NA>
958           <NA>        <NA>        <NA>
959           <NA>        <NA>        <NA>
960           <NA>        <NA>        <NA>
961           <NA>        <NA>        <NA>
962           <NA>        <NA>        <NA>
963           <NA>        <NA>        <NA>
964           <NA>        <NA>        <NA>
965           <NA>        <NA>        <NA>
966           <NA>        <NA>        <NA>
967           <NA>        <NA>        <NA>
968           <NA>        <NA>        <NA>
969           <NA>        <NA>        <NA>
970           <NA>        <NA>        <NA>
971           <NA>        <NA>        <NA>
972           <NA>        <NA>        <NA>
973           <NA>        <NA>        <NA>
974           <NA>        <NA>        <NA>
975           <NA>        <NA>        <NA>
976           <NA>        <NA>        <NA>
977           <NA>        <NA>        <NA>
978           <NA>        <NA>        <NA>
979           <NA>        <NA>        <NA>
980           <NA>        <NA>        <NA>
981           <NA>        <NA>        <NA>
982           <NA>        <NA>        <NA>
983           <NA>        <NA>        <NA>
984           <NA>        <NA>        <NA>
985           <NA>        <NA>        <NA>
986           <NA>        <NA>        <NA>
987           <NA>        <NA>        <NA>
988           <NA>        <NA>        <NA>
989           <NA>        <NA>        <NA>
990           <NA>        <NA>        <NA>
991           <NA>        <NA>        <NA>
992           <NA>        <NA>        <NA>
993           <NA>        <NA>        <NA>
994           <NA>        <NA>        <NA>
995           <NA>        <NA>        <NA>
996           <NA>        <NA>        <NA>
997           <NA>        <NA>        <NA>
998           <NA>        <NA>        <NA>
999           <NA>        <NA>        <NA>
1000          <NA>        <NA>        <NA>
1001          <NA>        <NA>        <NA>
1002          <NA>        <NA>        <NA>
1003          <NA>        <NA>        <NA>
1004 yellow throat        <NA>        <NA>
1005 yellow throat        <NA>        <NA>
1006 yellow throat        <NA>        <NA>
1007 yellow throat        <NA>        <NA>
1008          <NA>        <NA>        <NA>
1009          <NA>        <NA>        <NA>
1010          <NA>        <NA>        <NA>
1011          <NA>        <NA>        <NA>
1012          <NA>        <NA>        <NA>
1013          <NA>        <NA>        <NA>
1014          <NA>        <NA>        <NA>
1015          <NA>        <NA>        <NA>
1016          <NA>        <NA>        <NA>
1017          <NA>        <NA>        <NA>
1018          <NA>        <NA>        <NA>
1019          <NA>        <NA>        <NA>
1020          <NA>        <NA>        <NA>
1021          <NA>        <NA>        <NA>
1022          <NA>        <NA>        <NA>
1023          <NA>        <NA>        <NA>
1024          <NA>        <NA>        <NA>
1025          <NA>        <NA>        <NA>
1026          <NA>        <NA>        <NA>
1027          <NA>        <NA>        <NA>
1028          <NA>        <NA>        <NA>
1029          <NA>        <NA>        <NA>
1030          <NA>        <NA>        <NA>
1031          <NA>        <NA>        <NA>
1032          <NA>        <NA>        <NA>
1033          <NA>        <NA>        <NA>
1034          <NA>        <NA>        <NA>
1035          <NA>        <NA>        <NA>
1036          <NA>        <NA>        <NA>
1037          <NA>        <NA>        <NA>
1038          <NA>        <NA>        <NA>
1039          <NA>        <NA>        <NA>
1040          <NA>        <NA>        <NA>
1041          <NA>        <NA>        <NA>
1042          <NA>        <NA>        <NA>
1043          <NA>        <NA>        <NA>
1044          <NA>        <NA>        <NA>
1045          <NA>        <NA>        <NA>
1046          <NA>        <NA>        <NA>
1047          <NA>        <NA>        <NA>
1048          <NA>        <NA>        <NA>
1049          <NA>        <NA>        <NA>
1050          <NA>        <NA>        <NA>
1051          <NA>        <NA>        <NA>
1052          <NA>        <NA>        <NA>
1053          <NA>        <NA>        <NA>
1054          <NA>        <NA>        <NA>
1055          <NA>        <NA>        <NA>
1056          <NA>        <NA>        <NA>
1057          <NA>        <NA>        <NA>
1058          <NA>        <NA>        <NA>
1059          <NA>        <NA>        <NA>
1060          <NA>        <NA>        <NA>
1061          <NA>        <NA>        <NA>
1062          <NA>        <NA>        <NA>
1063          <NA>        <NA>        <NA>
1064          <NA>        <NA>        <NA>
1065          <NA>        <NA>        <NA>
1066          <NA>        <NA>        <NA>
1067          <NA>        <NA>        <NA>
1068          <NA>        <NA>        <NA>
1069          <NA>        <NA>        <NA>
1070          <NA>        <NA>        <NA>
1071          <NA>        <NA>        <NA>
1072          <NA>        <NA>        <NA>
1073          <NA>        <NA>        <NA>
1074          <NA>        <NA>        <NA>
1075          <NA>        <NA>        <NA>
1076          <NA>        <NA>        <NA>
1077          <NA>        <NA>        <NA>
1078          <NA>        <NA>        <NA>
1079          <NA>        <NA>        <NA>
1080          <NA>        <NA>        <NA>
1081          <NA>        <NA>        <NA>
1082          <NA>        <NA>        <NA>
1083          <NA>        <NA>        <NA>
1084          <NA>        <NA>        <NA>
1085          <NA>        <NA>        <NA>
1086          <NA>        <NA>        <NA>
1087          <NA>        <NA>        <NA>
1088          <NA>        <NA>        <NA>
1089          <NA>        <NA>        <NA>
1090          <NA>        <NA>        <NA>
1091          <NA>        <NA>        <NA>
1092          <NA>        <NA>        <NA>
1093          <NA>        <NA>        <NA>
1094          <NA>        <NA>        <NA>
1095          <NA>        <NA>        <NA>
1096          <NA>        <NA>        <NA>
1097          <NA>        <NA>        <NA>
1098          <NA>        <NA>        <NA>
1099          <NA>        <NA>        <NA>
1100          <NA>        <NA>        <NA>
1101          <NA>        <NA>        <NA>
1102          <NA>        <NA>        <NA>
1103          <NA>        <NA>        <NA>
1104          <NA>        <NA>        <NA>
1105          <NA>        <NA>        <NA>
1106          <NA>        <NA>        <NA>
1107          <NA>        <NA>        <NA>
1108          <NA>        <NA>        <NA>
1109          <NA>        <NA>        <NA>
1110          <NA>        <NA>        <NA>
1111          <NA>        <NA>        <NA>
1112          <NA>        <NA>        <NA>
1113          <NA>        <NA>        <NA>
1114          <NA>        <NA>        <NA>
1115          <NA>        <NA>        <NA>
1116          <NA>        <NA>        <NA>
1117          <NA>        <NA>        <NA>
1118          <NA>        <NA>        <NA>
1119          <NA>        <NA>        <NA>
1120          <NA>        <NA>        <NA>
1121          <NA>        <NA>        <NA>
1122          <NA>        <NA>        <NA>
1123          <NA>        <NA>        <NA>
1124          <NA>        <NA>        <NA>
1125          <NA>        <NA>        <NA>
1126          <NA>        <NA>        <NA>
1127          <NA>        <NA>        <NA>
1128          <NA>        <NA>        <NA>
1129          <NA>        <NA>        <NA>
1130          <NA>        <NA>        <NA>
1131          <NA>        <NA>        <NA>
1132          <NA>        <NA>        <NA>
1133          <NA>        <NA>        <NA>
1134          <NA>        <NA>        <NA>
1135          <NA>        <NA>        <NA>
1136          <NA>        <NA>        <NA>
1137          <NA>        <NA>        <NA>
1138          <NA>        <NA>        <NA>
1139          <NA>        <NA>        <NA>
1140          <NA>        <NA>        <NA>
1141          <NA>        <NA>        <NA>
1142          <NA>        <NA>        <NA>
1143          <NA>        <NA>        <NA>
1144          <NA>        <NA>        <NA>
1145          <NA>        <NA>        <NA>
1146          <NA>        <NA>        <NA>
1147          <NA>        <NA>        <NA>
1148          <NA>        <NA>        <NA>
1149          <NA>        <NA>        <NA>
1150          <NA>        <NA>        <NA>
1151          <NA>        <NA>        <NA>
1152          <NA>        <NA>        <NA>
1153          <NA>        <NA>        <NA>
1154          <NA>        <NA>        <NA>
1155          <NA>        <NA>        <NA>
1156          <NA>        <NA>        <NA>
1157          <NA>        <NA>        <NA>
1158          <NA>        <NA>        <NA>
1159          <NA>        <NA>        <NA>
1160          <NA>        <NA>        <NA>
1161          <NA>        <NA>        <NA>
1162          <NA>        <NA>        <NA>
1163          <NA>        <NA>        <NA>
1164          <NA>        <NA>        <NA>
1165          <NA>        <NA>        <NA>
1166          <NA>        <NA>        <NA>
1167          <NA>        <NA>        <NA>
1168          <NA>        <NA>        <NA>
1169          <NA>        <NA>        <NA>
1170          <NA>        <NA>        <NA>
1171          <NA>        <NA>        <NA>
1172          <NA>        <NA>        <NA>
1173          <NA>        <NA>        <NA>
1174          <NA>        <NA>        <NA>
1175          <NA>        <NA>        <NA>
1176          <NA>        <NA>        <NA>
1177          <NA>        <NA>        <NA>
1178          <NA>        <NA>        <NA>
1179          <NA>        <NA>        <NA>
1180          <NA>        <NA>        <NA>
1181          <NA>        <NA>        <NA>
1182          <NA>        <NA>        <NA>
1183          <NA>        <NA>        <NA>
1184          <NA>        <NA>        <NA>
1185          <NA>        <NA>        <NA>
1186          <NA>        <NA>        <NA>
1187          <NA>        <NA>        <NA>
1188          <NA>        <NA>        <NA>
1189          <NA>        <NA>        <NA>
1190          <NA>        <NA>        <NA>
1191          <NA>        <NA>        <NA>
1192          <NA>        <NA>        <NA>
1193          <NA>        <NA>        <NA>
1194          <NA>        <NA>        <NA>
1195          <NA>        <NA>        <NA>
1196          <NA>        <NA>        <NA>
1197          <NA>        <NA>        <NA>
1198          <NA>        <NA>        <NA>
1199          <NA>        <NA>        <NA>
1200          <NA>        <NA>        <NA>
1201          <NA>        <NA>        <NA>
1202          <NA>        <NA>        <NA>
1203          <NA>        <NA>        <NA>
1204          <NA>        <NA>        <NA>
1205          <NA>        <NA>        <NA>
1206          <NA>        <NA>        <NA>
1207          <NA>        <NA>        <NA>
1208          <NA>        <NA>        <NA>
1209          <NA>        <NA>        <NA>
1210          <NA>        <NA>        <NA>
1211          <NA>        <NA>        <NA>
1212          <NA>        <NA>        <NA>
1213          <NA>        <NA>        <NA>
1214          <NA>        <NA>        <NA>
1215          <NA>        <NA>        <NA>
1216          <NA>        <NA>        <NA>
1217          <NA>        <NA>        <NA>
1218          <NA>        <NA>        <NA>
1219          <NA>        <NA>        <NA>
1220          <NA>        <NA>        <NA>
1221          <NA>        <NA>        <NA>
1222          <NA>        <NA>        <NA>
1223          <NA>        <NA>        <NA>
1224          <NA>        <NA>        <NA>
1225          <NA>        <NA>        <NA>
1226          <NA>        <NA>        <NA>
1227          <NA>        <NA>        <NA>
1228          <NA>        <NA>        <NA>
1229          <NA>        <NA>        <NA>
1230          <NA>        <NA>        <NA>
1231          <NA>        <NA>        <NA>
1232          <NA>        <NA>        <NA>
1233          <NA>        <NA>        <NA>
1234          <NA>        <NA>        <NA>
1235          <NA>        <NA>        <NA>
1236          <NA>        <NA>        <NA>
1237          <NA>        <NA>        <NA>
1238          <NA>        <NA>        <NA>
1239          <NA>        <NA>        <NA>
1240          <NA>        <NA>        <NA>
1241          <NA>        <NA>        <NA>
1242          <NA>        <NA>        <NA>
1243          <NA>        <NA>        <NA>
1244          <NA>        <NA>        <NA>
1245          <NA>        <NA>        <NA>
1246          <NA>        <NA>        <NA>
1247          <NA>        <NA>        <NA>
1248          <NA>        <NA>        <NA>
1249          <NA>        <NA>        <NA>
1250          <NA>        <NA>        <NA>
1251          <NA>        <NA>        <NA>
1252          <NA>        <NA>        <NA>
1253          <NA>        <NA>        <NA>
1254          <NA>        <NA>        <NA>
1255          <NA>        <NA>        <NA>
1256          <NA>        <NA>        <NA>
1257          <NA>        <NA>        <NA>
1258          <NA>        <NA>        <NA>
1259          <NA>        <NA>        <NA>
1260          <NA>        <NA>        <NA>
1261          <NA>        <NA>        <NA>
1262          <NA>        <NA>        <NA>
1263          <NA>        <NA>        <NA>
1264          <NA>        <NA>        <NA>
1265          <NA>        <NA>        <NA>
1266          <NA>        <NA>        <NA>
1267          <NA>        <NA>        <NA>
1268          <NA>        <NA>        <NA>
1269          <NA>        <NA>        <NA>
1270          <NA>        <NA>        <NA>
1271          <NA>        <NA>        <NA>
1272          <NA>        <NA>        <NA>
1273          <NA>        <NA>        <NA>
1274          <NA>        <NA>        <NA>
1275          <NA>        <NA>        <NA>
1276          <NA>        <NA>        <NA>
1277          <NA>        <NA>        <NA>
1278          <NA>        <NA>        <NA>
1279          <NA>        <NA>        <NA>
1280          <NA>        <NA>        <NA>
1281          <NA>        <NA>        <NA>
1282          <NA>        <NA>        <NA>
1283          <NA>        <NA>        <NA>
1284          <NA>        <NA>        <NA>
1285          <NA>        <NA>        <NA>
1286          <NA>        <NA>        <NA>
1287          <NA>        <NA>        <NA>
1288          <NA>        <NA>        <NA>
1289          <NA>        <NA>        <NA>
1290          <NA>        <NA>        <NA>
1291          <NA>        <NA>        <NA>
1292          <NA>        <NA>        <NA>
1293          <NA>        <NA>        <NA>
1294          <NA>        <NA>        <NA>
1295          <NA>        <NA>        <NA>
1296          <NA>        <NA>        <NA>
1297          <NA>        <NA>        <NA>
1298          <NA>        <NA>        <NA>
1299          <NA>        <NA>        <NA>
1300          <NA>        <NA>        <NA>
1301          <NA>        <NA>        <NA>
1302          <NA>        <NA>        <NA>
1303          <NA>        <NA>        <NA>
1304          <NA>        <NA>        <NA>
1305          <NA>        <NA>        <NA>
1306          <NA>        <NA>        <NA>
1307          <NA>        <NA>        <NA>
1308          <NA>        <NA>        <NA>
1309          <NA>        <NA>        <NA>
1310          <NA>        <NA>        <NA>
1311          <NA>        <NA>        <NA>
1312          <NA>        <NA>        <NA>
1313          <NA>        <NA>        <NA>
1314          <NA>        <NA>        <NA>
1315          <NA>        <NA>        <NA>
1316          <NA>        <NA>        <NA>
1317          <NA>        <NA>        <NA>
1318          <NA>        <NA>        <NA>
1319          <NA>        <NA>        <NA>
1320          <NA>        <NA>        <NA>
1321          <NA>        <NA>        <NA>
1322          <NA>        <NA>        <NA>
1323          <NA>        <NA>        <NA>
1324          <NA>        <NA>        <NA>
1325          <NA>        <NA>        <NA>
1326          <NA>        <NA>        <NA>
1327          <NA>        <NA>        <NA>
1328          <NA>        <NA>        <NA>
1329          <NA>        <NA>        <NA>
1330          <NA>        <NA>        <NA>
1331          <NA>        <NA>        <NA>
1332          <NA>        <NA>        <NA>
1333          <NA>        <NA>        <NA>
1334          <NA>        <NA>        <NA>
1335          <NA>        <NA>        <NA>
1336          <NA>        <NA>        <NA>
1337          <NA>        <NA>        <NA>
1338          <NA>        <NA>        <NA>
1339          <NA>        <NA>        <NA>
1340          <NA>        <NA>        <NA>
1341          <NA>        <NA>        <NA>
1342          <NA>        <NA>        <NA>
1343          <NA>        <NA>        <NA>
1344          <NA>        <NA>        <NA>
1345          <NA>        <NA>        <NA>
1346          <NA>        <NA>        <NA>
1347          <NA>        <NA>        <NA>
1348          <NA>        <NA>        <NA>
1349          <NA>        <NA>        <NA>
1350          <NA>        <NA>        <NA>
1351          <NA>        <NA>        <NA>
1352          <NA>        <NA>        <NA>
1353          <NA>        <NA>        <NA>
1354          <NA>        <NA>        <NA>
1355          <NA>        <NA>        <NA>
1356          <NA>        <NA>        <NA>
1357          <NA>        <NA>        <NA>
1358          <NA>        <NA>        <NA>
1359          <NA>        <NA>        <NA>
1360          <NA>        <NA>        <NA>
1361          <NA>        <NA>        <NA>
1362          <NA>        <NA>        <NA>
1363          <NA>        <NA>        <NA>
1364          <NA>        <NA>        <NA>
1365          <NA>        <NA>        <NA>
1366          <NA>        <NA>        <NA>
1367          <NA>        <NA>        <NA>
1368          <NA>        <NA>        <NA>
1369          <NA>        <NA>        <NA>
1370          <NA>        <NA>        <NA>
1371          <NA>        <NA>        <NA>
1372          <NA>        <NA>        <NA>
1373          <NA>        <NA>        <NA>
1374          <NA>        <NA>        <NA>
1375          <NA>        <NA>        <NA>
1376          <NA>        <NA>        <NA>
1377          <NA>        <NA>        <NA>
1378        purple        pink        <NA>
1379        purple        pink        <NA>
1380        purple        pink        <NA>
1381        purple        pink        <NA>
1382        purple        pink        <NA>
1383        purple        pink        <NA>
1384        purple        pink        <NA>
1385        purple        pink        <NA>
1386        purple        pink        <NA>
1387        purple        pink        <NA>
1388          <NA>        <NA>        <NA>
1389          <NA>        <NA>        <NA>
1390          <NA>        <NA>        <NA>
1391          <NA>        <NA>        <NA>
1392          <NA>        <NA>        <NA>
1393          <NA>        <NA>        <NA>
1394          <NA>        <NA>        <NA>
1395          <NA>        <NA>        <NA>
1396          <NA>        <NA>        <NA>
1397          <NA>        <NA>        <NA>
1398          <NA>        <NA>        <NA>
1399          <NA>        <NA>        <NA>
1400          <NA>        <NA>        <NA>
1401          <NA>        <NA>        <NA>
1402          <NA>        <NA>        <NA>
1403          <NA>        <NA>        <NA>
1404          <NA>        <NA>        <NA>
1405          <NA>        <NA>        <NA>
1406          <NA>        <NA>        <NA>
1407          <NA>        <NA>        <NA>
1408          <NA>        <NA>        <NA>
1409          <NA>        <NA>        <NA>
1410          <NA>        <NA>        <NA>
1411          <NA>        <NA>        <NA>
1412          <NA>        <NA>        <NA>
1413          <NA>        <NA>        <NA>
1414          <NA>        <NA>        <NA>
1415          <NA>        <NA>        <NA>
1416          <NA>        <NA>        <NA>
1417          <NA>        <NA>        <NA>
1418          <NA>        <NA>        <NA>
1419          <NA>        <NA>        <NA>
1420          <NA>        <NA>        <NA>
1421          <NA>        <NA>        <NA>
1422          <NA>        <NA>        <NA>
1423          <NA>        <NA>        <NA>
1424          <NA>        <NA>        <NA>
1425          <NA>        <NA>        <NA>
1426          <NA>        <NA>        <NA>
1427          <NA>        <NA>        <NA>
1428          <NA>        <NA>        <NA>
1429          <NA>        <NA>        <NA>
1430          <NA>        <NA>        <NA>
1431          <NA>        <NA>        <NA>
1432          <NA>        <NA>        <NA>
1433          <NA>        <NA>        <NA>
1434          <NA>        <NA>        <NA>
1435          <NA>        <NA>        <NA>
1436          <NA>        <NA>        <NA>
1437          <NA>        <NA>        <NA>
1438          <NA>        <NA>        <NA>
1439          <NA>        <NA>        <NA>
1440          <NA>        <NA>        <NA>
1441          <NA>        <NA>        <NA>
1442          <NA>        <NA>        <NA>
1443          <NA>        <NA>        <NA>
1444          <NA>        <NA>        <NA>
1445          <NA>        <NA>        <NA>
1446          <NA>        <NA>        <NA>
1447          <NA>        <NA>        <NA>
1448          <NA>        <NA>        <NA>
1449          <NA>        <NA>        <NA>
1450          <NA>        <NA>        <NA>
1451          <NA>        <NA>        <NA>
1452          <NA>        <NA>        <NA>
1453          <NA>        <NA>        <NA>
1454          <NA>        <NA>        <NA>
1455          <NA>        <NA>        <NA>
1456          <NA>        <NA>        <NA>
1457          <NA>        <NA>        <NA>
1458          <NA>        <NA>        <NA>
1459          <NA>        <NA>        <NA>
1460          <NA>        <NA>        <NA>
1461          <NA>        <NA>        <NA>
1462          <NA>        <NA>        <NA>
1463          <NA>        <NA>        <NA>
1464          <NA>        <NA>        <NA>
1465          <NA>        <NA>        <NA>
1466          <NA>        <NA>        <NA>
1467          <NA>        <NA>        <NA>
1468          <NA>        <NA>        <NA>
1469          <NA>        <NA>        <NA>
1470          <NA>        <NA>        <NA>
1471          <NA>        <NA>        <NA>
1472          <NA>        <NA>        <NA>
1473          <NA>        <NA>        <NA>
1474          <NA>        <NA>        <NA>
1475          <NA>        <NA>        <NA>
1476          <NA>        <NA>        <NA>
1477          <NA>        <NA>        <NA>
1478          <NA>        <NA>        <NA>
1479          <NA>        <NA>        <NA>
1480          <NA>        <NA>        <NA>
1481          <NA>        <NA>        <NA>
1482          <NA>        <NA>        <NA>
1483          <NA>        <NA>        <NA>
1484          <NA>        <NA>        <NA>
1485          <NA>        <NA>        <NA>
1486          <NA>        <NA>        <NA>
1487          <NA>        <NA>        <NA>
1488          <NA>        <NA>        <NA>
1489          <NA>        <NA>        <NA>
1490          <NA>        <NA>        <NA>
1491          <NA>        <NA>        <NA>
1492          <NA>        <NA>        <NA>
1493          <NA>        <NA>        <NA>
1494          <NA>        <NA>        <NA>
1495          <NA>        <NA>        <NA>
1496          <NA>        <NA>        <NA>
1497          <NA>        <NA>        <NA>
1498          <NA>        <NA>        <NA>
1499          <NA>        <NA>        <NA>
1500          <NA>        <NA>        <NA>
1501          <NA>        <NA>        <NA>
1502          <NA>        <NA>        <NA>
1503          <NA>        <NA>        <NA>
1504          <NA>        <NA>        <NA>
1505          <NA>        <NA>        <NA>
1506          <NA>        <NA>        <NA>
1507          <NA>        <NA>        <NA>
1508          <NA>        <NA>        <NA>
1509          <NA>        <NA>        <NA>
1510          <NA>        <NA>        <NA>
1511          <NA>        <NA>        <NA>
1512          <NA>        <NA>        <NA>
1513          <NA>        <NA>        <NA>
1514          <NA>        <NA>        <NA>
1515          <NA>        <NA>        <NA>
1516          <NA>        <NA>        <NA>
1517          <NA>        <NA>        <NA>
1518          <NA>        <NA>        <NA>
1519          <NA>        <NA>        <NA>
1520          pink      purple      yellow
1521          pink      purple      yellow
1522          pink      purple      yellow
1523          pink      purple      yellow
1524          pink      purple      yellow
1525          pink      purple      yellow
1526          pink      purple      yellow
1527          <NA>        <NA>        <NA>
1528          <NA>        <NA>        <NA>
1529          <NA>        <NA>        <NA>
1530          <NA>        <NA>        <NA>
1531          <NA>        <NA>        <NA>
1532          <NA>        <NA>        <NA>
1533          <NA>        <NA>        <NA>
1534          <NA>        <NA>        <NA>
1535          <NA>        <NA>        <NA>
1536          <NA>        <NA>        <NA>
1537          <NA>        <NA>        <NA>
1538          <NA>        <NA>        <NA>
1539          <NA>        <NA>        <NA>
1540          <NA>        <NA>        <NA>
1541          <NA>        <NA>        <NA>
1542          <NA>        <NA>        <NA>
1543          <NA>        <NA>        <NA>
1544          <NA>        <NA>        <NA>
1545          <NA>        <NA>        <NA>
1546          <NA>        <NA>        <NA>
1547          <NA>        <NA>        <NA>
1548          <NA>        <NA>        <NA>
1549          <NA>        <NA>        <NA>
1550          <NA>        <NA>        <NA>
1551          <NA>        <NA>        <NA>
1552          <NA>        <NA>        <NA>
1553          <NA>        <NA>        <NA>
1554          <NA>        <NA>        <NA>
1555          <NA>        <NA>        <NA>
1556          <NA>        <NA>        <NA>
1557          <NA>        <NA>        <NA>
1558          <NA>        <NA>        <NA>
1559          <NA>        <NA>        <NA>
1560          <NA>        <NA>        <NA>
1561          <NA>        <NA>        <NA>
1562          <NA>        <NA>        <NA>
1563          <NA>        <NA>        <NA>
1564          <NA>        <NA>        <NA>
1565          <NA>        <NA>        <NA>
1566          <NA>        <NA>        <NA>
1567          <NA>        <NA>        <NA>
1568          <NA>        <NA>        <NA>
1569          <NA>        <NA>        <NA>
1570          <NA>        <NA>        <NA>
1571          <NA>        <NA>        <NA>
1572          <NA>        <NA>        <NA>
1573          <NA>        <NA>        <NA>
1574          <NA>        <NA>        <NA>
1575          <NA>        <NA>        <NA>
1576          <NA>        <NA>        <NA>
1577          <NA>        <NA>        <NA>
1578          <NA>        <NA>        <NA>
1579          <NA>        <NA>        <NA>
1580          <NA>        <NA>        <NA>
1581          <NA>        <NA>        <NA>
1582          <NA>        <NA>        <NA>
1583          <NA>        <NA>        <NA>
1584          <NA>        <NA>        <NA>
1585          <NA>        <NA>        <NA>
1586          <NA>        <NA>        <NA>
1587          <NA>        <NA>        <NA>
1588          <NA>        <NA>        <NA>
1589          <NA>        <NA>        <NA>
1590          <NA>        <NA>        <NA>
1591          <NA>        <NA>        <NA>
1592          <NA>        <NA>        <NA>
1593          <NA>        <NA>        <NA>
1594          <NA>        <NA>        <NA>
1595          <NA>        <NA>        <NA>
1596          <NA>        <NA>        <NA>
1597          <NA>        <NA>        <NA>
1598          <NA>        <NA>        <NA>
1599          <NA>        <NA>        <NA>
1600          <NA>        <NA>        <NA>
1601          <NA>        <NA>        <NA>
1602          <NA>        <NA>        <NA>
1603          <NA>        <NA>        <NA>
1604          <NA>        <NA>        <NA>
1605          <NA>        <NA>        <NA>
1606          <NA>        <NA>        <NA>
1607          <NA>        <NA>        <NA>
1608          <NA>        <NA>        <NA>
1609          <NA>        <NA>        <NA>
1610          <NA>        <NA>        <NA>
1611          <NA>        <NA>        <NA>
1612          <NA>        <NA>        <NA>
1613          <NA>        <NA>        <NA>
1614          <NA>        <NA>        <NA>
1615          <NA>        <NA>        <NA>
1616          <NA>        <NA>        <NA>
1617          <NA>        <NA>        <NA>
1618          <NA>        <NA>        <NA>
1619          <NA>        <NA>        <NA>
1620          <NA>        <NA>        <NA>
1621          <NA>        <NA>        <NA>
1622          <NA>        <NA>        <NA>
1623          <NA>        <NA>        <NA>
1624          <NA>        <NA>        <NA>
1625          <NA>        <NA>        <NA>
1626          <NA>        <NA>        <NA>
1627          <NA>        <NA>        <NA>
1628          <NA>        <NA>        <NA>
1629          <NA>        <NA>        <NA>
1630          <NA>        <NA>        <NA>
1631          <NA>        <NA>        <NA>
1632          <NA>        <NA>        <NA>
1633          <NA>        <NA>        <NA>
1634          <NA>        <NA>        <NA>
1635          <NA>        <NA>        <NA>
1636          <NA>        <NA>        <NA>
1637          <NA>        <NA>        <NA>
1638          <NA>        <NA>        <NA>
1639          <NA>        <NA>        <NA>
1640          <NA>        <NA>        <NA>
1641          <NA>        <NA>        <NA>
1642          <NA>        <NA>        <NA>
1643          <NA>        <NA>        <NA>
1644          <NA>        <NA>        <NA>
1645          <NA>        <NA>        <NA>
1646          <NA>        <NA>        <NA>
1647          <NA>        <NA>        <NA>
1648          <NA>        <NA>        <NA>
1649          <NA>        <NA>        <NA>
1650          <NA>        <NA>        <NA>
1651          <NA>        <NA>        <NA>
1652          <NA>        <NA>        <NA>
1653          <NA>        <NA>        <NA>
1654          <NA>        <NA>        <NA>
1655          <NA>        <NA>        <NA>
1656          <NA>        <NA>        <NA>
1657          <NA>        <NA>        <NA>
1658          <NA>        <NA>        <NA>
1659          <NA>        <NA>        <NA>
1660          <NA>        <NA>        <NA>
1661          <NA>        <NA>        <NA>
1662          <NA>        <NA>        <NA>
1663          <NA>        <NA>        <NA>
1664          <NA>        <NA>        <NA>
1665          <NA>        <NA>        <NA>
1666          <NA>        <NA>        <NA>
1667          <NA>        <NA>        <NA>
1668          <NA>        <NA>        <NA>
1669          <NA>        <NA>        <NA>
1670          <NA>        <NA>        <NA>
1671          <NA>        <NA>        <NA>
1672  white (both)        <NA>        <NA>
1673  white (both)        <NA>        <NA>
1674  white (both)        <NA>        <NA>
1675  white (both)        <NA>        <NA>
1676  white (both)        <NA>        <NA>
1677  white (both)        <NA>        <NA>
1678  white (both)        <NA>        <NA>
1679          <NA>        <NA>        <NA>
1680          <NA>        <NA>        <NA>
1681          <NA>        <NA>        <NA>
1682          <NA>        <NA>        <NA>
1683          <NA>        <NA>        <NA>
1684          <NA>        <NA>        <NA>
1685          <NA>        <NA>        <NA>
1686          <NA>        <NA>        <NA>
1687          <NA>        <NA>        <NA>
1688          <NA>        <NA>        <NA>
1689          <NA>        <NA>        <NA>
1690          <NA>        <NA>        <NA>
1691          <NA>        <NA>        <NA>
1692          <NA>        <NA>        <NA>
1693          <NA>        <NA>        <NA>
1694          <NA>        <NA>        <NA>
1695          <NA>        <NA>        <NA>
1696          <NA>        <NA>        <NA>
1697          <NA>        <NA>        <NA>
1698          <NA>        <NA>        <NA>
1699          <NA>        <NA>        <NA>
1700          <NA>        <NA>        <NA>
1701          <NA>        <NA>        <NA>
1702          <NA>        <NA>        <NA>
1703          <NA>        <NA>        <NA>
1704          <NA>        <NA>        <NA>
1705          <NA>        <NA>        <NA>
1706          <NA>        <NA>        <NA>
1707          <NA>        <NA>        <NA>
1708          <NA>        <NA>        <NA>
1709          <NA>        <NA>        <NA>
1710          <NA>        <NA>        <NA>
1711          <NA>        <NA>        <NA>
1712          <NA>        <NA>        <NA>
1713          <NA>        <NA>        <NA>
1714          <NA>        <NA>        <NA>
1715          <NA>        <NA>        <NA>
1716          <NA>        <NA>        <NA>
1717          <NA>        <NA>        <NA>
1718          <NA>        <NA>        <NA>
1719          <NA>        <NA>        <NA>
1720          <NA>        <NA>        <NA>
1721          <NA>        <NA>        <NA>
1722          <NA>        <NA>        <NA>
1723          <NA>        <NA>        <NA>
1724          <NA>        <NA>        <NA>
1725          <NA>        <NA>        <NA>
1726          <NA>        <NA>        <NA>
1727          <NA>        <NA>        <NA>
1728          <NA>        <NA>        <NA>
1729          <NA>        <NA>        <NA>
1730          <NA>        <NA>        <NA>
1731          <NA>        <NA>        <NA>
1732          <NA>        <NA>        <NA>
1733          <NA>        <NA>        <NA>
1734          <NA>        <NA>        <NA>
1735          <NA>        <NA>        <NA>
1736          <NA>        <NA>        <NA>
1737          <NA>        <NA>        <NA>
1738          <NA>        <NA>        <NA>
1739          <NA>        <NA>        <NA>
1740          <NA>        <NA>        <NA>
1741          <NA>        <NA>        <NA>
1742          <NA>        <NA>        <NA>
1743          <NA>        <NA>        <NA>
1744          <NA>        <NA>        <NA>
1745          <NA>        <NA>        <NA>
1746          <NA>        <NA>        <NA>
1747          <NA>        <NA>        <NA>
1748          <NA>        <NA>        <NA>
1749          <NA>        <NA>        <NA>
1750          <NA>        <NA>        <NA>
1751          <NA>        <NA>        <NA>
1752          <NA>        <NA>        <NA>
1753          <NA>        <NA>        <NA>
1754          <NA>        <NA>        <NA>
1755          <NA>        <NA>        <NA>
1756          <NA>        <NA>        <NA>
1757          <NA>        <NA>        <NA>
1758          <NA>        <NA>        <NA>
1759          <NA>        <NA>        <NA>
1760          <NA>        <NA>        <NA>
1761          <NA>        <NA>        <NA>
1762          <NA>        <NA>        <NA>
1763          <NA>        <NA>        <NA>
1764          <NA>        <NA>        <NA>
1765          <NA>        <NA>        <NA>
1766          <NA>        <NA>        <NA>
1767          <NA>        <NA>        <NA>
1768          <NA>        <NA>        <NA>
1769          <NA>        <NA>        <NA>
1770          <NA>        <NA>        <NA>
1771          <NA>        <NA>        <NA>
1772          <NA>        <NA>        <NA>
1773          <NA>        <NA>        <NA>
1774          <NA>        <NA>        <NA>
1775          <NA>        <NA>        <NA>
1776          <NA>        <NA>        <NA>
1777          <NA>        <NA>        <NA>
1778          <NA>        <NA>        <NA>
1779          <NA>        <NA>        <NA>
1780          <NA>        <NA>        <NA>
1781          <NA>        <NA>        <NA>
1782          <NA>        <NA>        <NA>
1783          <NA>        <NA>        <NA>
1784          <NA>        <NA>        <NA>
1785          <NA>        <NA>        <NA>
1786          <NA>        <NA>        <NA>
1787          <NA>        <NA>        <NA>
1788          <NA>        <NA>        <NA>
1789          <NA>        <NA>        <NA>
1790          <NA>        <NA>        <NA>
1791          <NA>        <NA>        <NA>
1792          <NA>        <NA>        <NA>
1793          <NA>        <NA>        <NA>
1794          <NA>        <NA>        <NA>
1795          <NA>        <NA>        <NA>
1796          <NA>        <NA>        <NA>
1797          <NA>        <NA>        <NA>
1798          <NA>        <NA>        <NA>
1799          <NA>        <NA>        <NA>
1800          <NA>        <NA>        <NA>
1801          <NA>        <NA>        <NA>
1802          <NA>        <NA>        <NA>
1803          <NA>        <NA>        <NA>
1804          <NA>        <NA>        <NA>
1805          <NA>        <NA>        <NA>
1806          <NA>        <NA>        <NA>
1807          <NA>        <NA>        <NA>
1808          <NA>        <NA>        <NA>
1809          <NA>        <NA>        <NA>
1810          <NA>        <NA>        <NA>
1811          <NA>        <NA>        <NA>
1812          <NA>        <NA>        <NA>
1813          <NA>        <NA>        <NA>
1814          <NA>        <NA>        <NA>
1815          <NA>        <NA>        <NA>
1816          <NA>        <NA>        <NA>
1817          <NA>        <NA>        <NA>
1818          <NA>        <NA>        <NA>
1819          <NA>        <NA>        <NA>
1820          <NA>        <NA>        <NA>
1821          <NA>        <NA>        <NA>
1822          <NA>        <NA>        <NA>
1823          <NA>        <NA>        <NA>
1824          <NA>        <NA>        <NA>
1825          <NA>        <NA>        <NA>
1826          <NA>        <NA>        <NA>
1827          <NA>        <NA>        <NA>
1828          <NA>        <NA>        <NA>
1829          <NA>        <NA>        <NA>
1830          <NA>        <NA>        <NA>
1831          <NA>        <NA>        <NA>
1832          <NA>        <NA>        <NA>
1833         white        <NA>        <NA>
1834         white        <NA>        <NA>
1835         white        <NA>        <NA>
1836         white        <NA>        <NA>
1837         white        <NA>        <NA>
1838         white        <NA>        <NA>
1839         white        <NA>        <NA>
1840         white        <NA>        <NA>
1841         white        <NA>        <NA>
1842          <NA>        <NA>        <NA>
1843          <NA>        <NA>        <NA>
1844          <NA>        <NA>        <NA>
1845          <NA>        <NA>        <NA>
1846          <NA>        <NA>        <NA>
1847          <NA>        <NA>        <NA>
1848          <NA>        <NA>        <NA>
1849          <NA>        <NA>        <NA>
1850          <NA>        <NA>        <NA>
1851          <NA>        <NA>        <NA>
1852          <NA>        <NA>        <NA>
1853          <NA>        <NA>        <NA>
1854          <NA>        <NA>        <NA>
1855          <NA>        <NA>        <NA>
1856          <NA>        <NA>        <NA>
1857          <NA>        <NA>        <NA>
1858          <NA>        <NA>        <NA>
1859          <NA>        <NA>        <NA>
1860          <NA>        <NA>        <NA>
1861          <NA>        <NA>        <NA>
1862          <NA>        <NA>        <NA>
1863          <NA>        <NA>        <NA>
1864          <NA>        <NA>        <NA>
1865          <NA>        <NA>        <NA>
1866          <NA>        <NA>        <NA>
1867          <NA>        <NA>        <NA>
1868          <NA>        <NA>        <NA>
1869          <NA>        <NA>        <NA>
1870          <NA>        <NA>        <NA>
1871          <NA>        <NA>        <NA>
1872          <NA>        <NA>        <NA>
1873          <NA>        <NA>        <NA>
1874          <NA>        <NA>        <NA>
1875          <NA>        <NA>        <NA>
1876          <NA>        <NA>        <NA>
1877          <NA>        <NA>        <NA>
1878          <NA>        <NA>        <NA>
1879          <NA>        <NA>        <NA>
1880          <NA>        <NA>        <NA>
1881          <NA>        <NA>        <NA>
1882          <NA>        <NA>        <NA>
1883          <NA>        <NA>        <NA>
1884          <NA>        <NA>        <NA>
1885          <NA>        <NA>        <NA>
1886          <NA>        <NA>        <NA>
1887          <NA>        <NA>        <NA>
1888          <NA>        <NA>        <NA>
1889          <NA>        <NA>        <NA>
1890          <NA>        <NA>        <NA>
1891          <NA>        <NA>        <NA>
1892          <NA>        <NA>        <NA>
1893          <NA>        <NA>        <NA>
1894          <NA>        <NA>        <NA>
1895          <NA>        <NA>        <NA>
1896          <NA>        <NA>        <NA>
1897          <NA>        <NA>        <NA>
1898          <NA>        <NA>        <NA>
1899          <NA>        <NA>        <NA>
1900          <NA>        <NA>        <NA>
1901          <NA>        <NA>        <NA>
1902          <NA>        <NA>        <NA>
1903          <NA>        <NA>        <NA>
1904          <NA>        <NA>        <NA>
1905          <NA>        <NA>        <NA>
1906          <NA>        <NA>        <NA>
1907          <NA>        <NA>        <NA>
1908          <NA>        <NA>        <NA>
1909          <NA>        <NA>        <NA>
1910          <NA>        <NA>        <NA>
1911          <NA>        <NA>        <NA>
1912          <NA>        <NA>        <NA>
1913          <NA>        <NA>        <NA>
1914          <NA>        <NA>        <NA>
1915          <NA>        <NA>        <NA>
1916          <NA>        <NA>        <NA>
1917          <NA>        <NA>        <NA>
1918          <NA>        <NA>        <NA>
1919          <NA>        <NA>        <NA>
1920          <NA>        <NA>        <NA>
1921          <NA>        <NA>        <NA>
1922          <NA>        <NA>        <NA>
1923          <NA>        <NA>        <NA>
1924          <NA>        <NA>        <NA>
1925          <NA>        <NA>        <NA>
1926          <NA>        <NA>        <NA>
1927          <NA>        <NA>        <NA>
1928          <NA>        <NA>        <NA>
1929          <NA>        <NA>        <NA>
1930          <NA>        <NA>        <NA>
1931          <NA>        <NA>        <NA>
1932          <NA>        <NA>        <NA>
1933          <NA>        <NA>        <NA>
1934          <NA>        <NA>        <NA>
1935          <NA>        <NA>        <NA>
1936          <NA>        <NA>        <NA>
1937          <NA>        <NA>        <NA>
1938          <NA>        <NA>        <NA>
1939          <NA>        <NA>        <NA>
1940          <NA>        <NA>        <NA>
1941          <NA>        <NA>        <NA>
1942          <NA>        <NA>        <NA>
1943          <NA>        <NA>        <NA>
1944          <NA>        <NA>        <NA>
1945          pink   pale blue        <NA>
1946          <NA>        <NA>        <NA>
1947          <NA>        <NA>        <NA>
1948          <NA>        <NA>        <NA>
1949          <NA>        <NA>        <NA>
1950          <NA>        <NA>        <NA>
1951          <NA>        <NA>        <NA>
1952          <NA>        <NA>        <NA>
1953          <NA>        <NA>        <NA>
1954          <NA>        <NA>        <NA>
1955          <NA>        <NA>        <NA>
1956          <NA>        <NA>        <NA>
1957          <NA>        <NA>        <NA>
1958          <NA>        <NA>        <NA>
1959          <NA>        <NA>        <NA>
1960          <NA>        <NA>        <NA>
1961          <NA>        <NA>        <NA>
1962          <NA>        <NA>        <NA>
1963          <NA>        <NA>        <NA>
1964          <NA>        <NA>        <NA>
1965          <NA>        <NA>        <NA>
1966          <NA>        <NA>        <NA>
1967          <NA>        <NA>        <NA>
1968          <NA>        <NA>        <NA>
1969          <NA>        <NA>        <NA>
1970          <NA>        <NA>        <NA>
1971          <NA>        <NA>        <NA>
1972          <NA>        <NA>        <NA>
1973          <NA>        <NA>        <NA>
1974          <NA>        <NA>        <NA>
1975          <NA>        <NA>        <NA>
1976          <NA>        <NA>        <NA>
1977          <NA>        <NA>        <NA>
1978          <NA>        <NA>        <NA>
1979          <NA>        <NA>        <NA>
1980          <NA>        <NA>        <NA>
1981          <NA>        <NA>        <NA>
1982          <NA>        <NA>        <NA>
1983          <NA>        <NA>        <NA>
1984          <NA>        <NA>        <NA>
1985          <NA>        <NA>        <NA>
1986          <NA>        <NA>        <NA>
1987          pink        <NA>        <NA>
1988          pink        <NA>        <NA>
1989          pink        <NA>        <NA>
1990          pink        <NA>        <NA>
1991          pink        <NA>        <NA>
1992          pink        <NA>        <NA>
1993          pink        <NA>        <NA>
1994          <NA>        <NA>        <NA>
1995          <NA>        <NA>        <NA>
1996          <NA>        <NA>        <NA>
1997          pink       white        <NA>
1998          pink       white        <NA>
1999          pink       white        <NA>
2000          pink       white        <NA>
2001          pink       white        <NA>
2002          pink       white        <NA>
2003          pink       white        <NA>
2004          pink        <NA>        <NA>
2005          pink        <NA>        <NA>
2006          pink        <NA>        <NA>
2007          pink        <NA>        <NA>
2008          pink        <NA>        <NA>
2009          pink        <NA>        <NA>
2010          pink        <NA>        <NA>
2011          pink        <NA>        <NA>
2012          pink        <NA>        <NA>
2013          pink        <NA>        <NA>
2014          pink        <NA>        <NA>
2015          pink        <NA>        <NA>
2016          pink        <NA>        <NA>
2017          pink        <NA>        <NA>
2018          pink        <NA>        <NA>
2019          pink        <NA>        <NA>
2020          pink        <NA>        <NA>
2021          pink        <NA>        <NA>
2022          pink        <NA>        <NA>
2023          pink        <NA>        <NA>
2024          pink        <NA>        <NA>
2025          pink        <NA>        <NA>
2026         white        <NA>        <NA>
2027         white        <NA>        <NA>
2028         white        <NA>        <NA>
2029     pale blue        <NA>        <NA>
2030     pale blue        <NA>        <NA>
2031     pale blue        <NA>        <NA>
2032     pale blue        <NA>        <NA>
2033     pale blue        <NA>        <NA>
2034     pale blue        <NA>        <NA>
2035     pale blue        <NA>        <NA>
2036     pale blue        <NA>        <NA>
2037     pale blue        <NA>        <NA>
2038     pale blue        <NA>        <NA>
2039          pink        <NA>        <NA>
2040          pink        <NA>        <NA>
2041          pink        <NA>        <NA>
2042          pink        <NA>        <NA>
2043          pink        <NA>        <NA>
2044          pink        <NA>        <NA>
2045          pink        <NA>        <NA>
2046          pink        <NA>        <NA>
2047          pink        <NA>        <NA>
2048          pink        <NA>        <NA>
2049          <NA>        <NA>        <NA>
2050          <NA>        <NA>        <NA>
2051          <NA>        <NA>        <NA>
2052          <NA>        <NA>        <NA>
2053          <NA>        <NA>        <NA>
2054          <NA>        <NA>        <NA>
2055          <NA>        <NA>        <NA>
2056          pink        <NA>        <NA>
2057          pink        <NA>        <NA>
2058          pink        <NA>        <NA>
2059          pink        <NA>        <NA>
2060          pink        <NA>        <NA>
2061          pink        <NA>        <NA>
2062          pink        <NA>        <NA>
2063         white        <NA>        <NA>
2064         white        <NA>        <NA>
2065         white        <NA>        <NA>
2066         white        <NA>        <NA>
2067         white        <NA>        <NA>
2068         white        <NA>        <NA>
2069         white        <NA>        <NA>
2070          pink        <NA>        <NA>
2071          pink        <NA>        <NA>
2072          pink        <NA>        <NA>
2073          pink        <NA>        <NA>
2074          pink        <NA>        <NA>
2075          pink        <NA>        <NA>
2076          pink        <NA>        <NA>
2077          pink        <NA>        <NA>
2078          pink        <NA>        <NA>
2079          pink        <NA>        <NA>
2080          <NA>        <NA>        <NA>
2081          <NA>        <NA>        <NA>
2082          <NA>        <NA>        <NA>
2083          <NA>        <NA>        <NA>
2084          <NA>        <NA>        <NA>
2085          <NA>        <NA>        <NA>
2086          <NA>        <NA>        <NA>
2087          <NA>        <NA>        <NA>
2088          <NA>        <NA>        <NA>
2089          <NA>        <NA>        <NA>
2090          <NA>        <NA>        <NA>
2091          <NA>        <NA>        <NA>
2092          <NA>        <NA>        <NA>
2093          <NA>        <NA>        <NA>
2094          <NA>        <NA>        <NA>
2095          <NA>        <NA>        <NA>
2096          <NA>        <NA>        <NA>
2097          <NA>        <NA>        <NA>
2098          <NA>        <NA>        <NA>
2099          <NA>        <NA>        <NA>
2100          <NA>        <NA>        <NA>
2101          <NA>        <NA>        <NA>
2102          <NA>        <NA>        <NA>
2103          <NA>        <NA>        <NA>
2104          <NA>        <NA>        <NA>
2105          <NA>        <NA>        <NA>
2106          <NA>        <NA>        <NA>
2107          <NA>        <NA>        <NA>
2108          <NA>        <NA>        <NA>
2109          <NA>        <NA>        <NA>
2110          pink       white        <NA>
2111          pink       white        <NA>
2112          pink       white        <NA>
2113          pink       white        <NA>
2114          pink       white        <NA>
2115          pink       white        <NA>
2116          pink       white        <NA>
2117          pink       white        <NA>
2118          pink       white        <NA>
2119          pink       white        <NA>
2120          <NA>        <NA>        <NA>
2121          <NA>        <NA>        <NA>
2122          <NA>        <NA>        <NA>
2123          <NA>        <NA>        <NA>
2124          <NA>        <NA>        <NA>
2125         white        <NA>        <NA>
2126         white        <NA>        <NA>
2127         white        <NA>        <NA>
2128         white        <NA>        <NA>
2129         white        <NA>        <NA>
2130         white        <NA>        <NA>
2131         white        <NA>        <NA>
2132         white        <NA>        <NA>
2133         white        <NA>        <NA>
2134         white        <NA>        <NA>
2135         white        <NA>        <NA>
2136         white        <NA>        <NA>
2137         white        <NA>        <NA>
2138         white        <NA>        <NA>
2139         white        <NA>        <NA>
2140         white        <NA>        <NA>
2141         white        <NA>        <NA>
2142         white        <NA>        <NA>
2143         white        <NA>        <NA>
2144         white        <NA>        <NA>
2145         white        <NA>        <NA>
2146         white        <NA>        <NA>
2147         white        <NA>        <NA>
2148         white        <NA>        <NA>
2149          pink       white        <NA>
2150          pink       white        <NA>
2151          pink       white        <NA>
2152          pink       white        <NA>
2153          pink       white        <NA>
2154          pink       white        <NA>
2155          pink       white        <NA>
2156          pink       white        <NA>
2157          pink       white        <NA>
2158          pink       white        <NA>
2159          pink       white        <NA>
2160          pink       white        <NA>
2161          pink       white        <NA>
2162          pink       white        <NA>
2163          pink        <NA>        <NA>
2164          pink        <NA>        <NA>
2165          pink        <NA>        <NA>
2166          pink        <NA>        <NA>
2167          pink        <NA>        <NA>
2168          pink        <NA>        <NA>
2169          pink        <NA>        <NA>
2170          pink        <NA>        <NA>
2171          pink        <NA>        <NA>
2172          pink        <NA>        <NA>
2173          pink        <NA>        <NA>
2174          pink        <NA>        <NA>
2175          pink        <NA>        <NA>
2176          pink        <NA>        <NA>
2177          pink        <NA>        <NA>
2178          pink        <NA>        <NA>
2179          pink        <NA>        <NA>
2180          pink        <NA>        <NA>
2181          pink        <NA>        <NA>
2182          pink        <NA>        <NA>
2183          pink        <NA>        <NA>
2184          pink        <NA>        <NA>
2185          pink        <NA>        <NA>
2186          pink        <NA>        <NA>
2187          pink        <NA>        <NA>
2188          pink        <NA>        <NA>
2189          pink        <NA>        <NA>
2190          pink        <NA>        <NA>
2191          pink        <NA>        <NA>
2192          pink        <NA>        <NA>
2193          pink        <NA>        <NA>
2194          pink        <NA>        <NA>
2195          pink        <NA>        <NA>
2196          pink        <NA>        <NA>
2197          pink        <NA>        <NA>
2198          pink        <NA>        <NA>
2199          pink        <NA>        <NA>
2200          pink        <NA>        <NA>
2201          pink        <NA>        <NA>
2202          pink       white        <NA>
2203          pink       white        <NA>
2204          pink       white        <NA>
2205          pink       white        <NA>
2206          pink       white        <NA>
2207          pink       white        <NA>
2208          pink       white        <NA>
2209          pink       white        <NA>
2210          pink       white        <NA>
2211          pink       white        <NA>
2212          pink       white        <NA>
2213          pink       white        <NA>
2214          pink       white        <NA>
2215          pink       white        <NA>
2216          pink       white        <NA>
2217          pink       white        <NA>
2218          pink        <NA>        <NA>
2219          pink        <NA>        <NA>
2220          pink        <NA>        <NA>
2221          pink        <NA>        <NA>
2222         white        <NA>        <NA>
2223         white        <NA>        <NA>
2224         white        <NA>        <NA>
2225         white        <NA>        <NA>
2226          pink       white        <NA>
2227          pink       white        <NA>
2228          pink       white        <NA>
2229          pink       white        <NA>
2230          pink       white        <NA>
2231          pink       white        <NA>
2232          pink       white        <NA>
2233          pink       white        <NA>
2234          pink       white        <NA>
2235          pink       white        <NA>
2236          pink       white        <NA>
2237          pink       white        <NA>
2238          pink       white        <NA>
2239          pink       white        <NA>
2240          pink       white        <NA>
2241          pink       white        <NA>
2242          pink       white        <NA>
2243          pink       white        <NA>
2244          pink       white        <NA>
2245          pink       white        <NA>
2246          pink       white        <NA>
2247          pink       white        <NA>
2248          pink       white        <NA>
2249          pink       white        <NA>
2250          pink       white        <NA>
2251          pink       white        <NA>
2252          pink       white        <NA>
2253          pink       white        <NA>
2254          pink       white        <NA>
2255          pink       white        <NA>
2256          pink       white        <NA>
2257          pink       white        <NA>
2258          pink       white        <NA>
2259          <NA>        <NA>        <NA>
2260          <NA>        <NA>        <NA>
2261          <NA>        <NA>        <NA>
2262          <NA>        <NA>        <NA>
2263          <NA>        <NA>        <NA>
2264          <NA>        <NA>        <NA>
2265          <NA>        <NA>        <NA>
2266          <NA>        <NA>        <NA>
2267          <NA>        <NA>        <NA>
2268          <NA>        <NA>        <NA>
2269          pink       white        <NA>
2270          pink       white        <NA>
2271          pink       white        <NA>
2272          pink       white        <NA>
2273          pink       white        <NA>
2274          pink       white        <NA>
2275          pink       white        <NA>
2276          pink       white        <NA>
2277          pink       white        <NA>
2278          pink       white        <NA>
2279          pink       white        <NA>
2280          pink       white        <NA>
2281          pink       white        <NA>
2282          pink       white        <NA>
2283          pink       white        <NA>
2284          pink       white        <NA>
2285          pink       white        <NA>
2286          pink       white        <NA>
2287          pink       white        <NA>
2288          pink       white        <NA>
2289          pink       white        <NA>
2290          pink       white        <NA>
2291          pink       white        <NA>
2292          pink       white        <NA>
2293          pink       white        <NA>
2294          pink       white        <NA>
2295          pink       white        <NA>
2296          pink       white        <NA>
2297          pink       white        <NA>
2298          pink       white        <NA>
2299          pink       white        <NA>
2300          pink       white        <NA>
2301          pink       white        <NA>
2302          pink       white        <NA>
2303          pink       white        <NA>
2304          pink       white        <NA>
2305          pink       white        <NA>
2306          pink       white        <NA>
2307          pink       white        <NA>
2308          pink       white        <NA>
2309          pink       white        <NA>
2310          pink       white        <NA>
2311          <NA>        <NA>        <NA>
2312          <NA>        <NA>        <NA>
2313          <NA>        <NA>        <NA>
2314          <NA>        <NA>        <NA>
2315          pink       white        <NA>
2316          pink       white        <NA>
2317          pink       white        <NA>
2318          pink       white        <NA>
2319          pink       white        <NA>
2320          pink       white        <NA>
2321          pink       white        <NA>
2322          pink       white        <NA>
2323          pink       white        <NA>
2324          pink       white        <NA>
2325          pink       white        <NA>
2326          pink       white        <NA>
2327          pink       white        <NA>
2328          pink       white        <NA>
2329          pink       white        <NA>
2330          pink       white        <NA>
2331          pink       white        <NA>
2332          pink       white        <NA>
2333          pink       white        <NA>
2334          pink       white        <NA>
2335          pink       white        <NA>
2336          pink       white        <NA>
2337          pink       white        <NA>
2338          pink       white        <NA>
2339          pink       white        <NA>
2340          pink       white        <NA>
2341          <NA>        <NA>        <NA>
2342          <NA>        <NA>        <NA>
2343          <NA>        <NA>        <NA>
2344          pink       white        <NA>
2345          pink       white        <NA>
2346          pink       white        <NA>
2347          pink       white        <NA>
2348          pink       white        <NA>
2349          pink       white        <NA>
2350          pink       white        <NA>
2351          pink       white        <NA>
2352          pink       white        <NA>
2353          pink       white        <NA>
2354          <NA>        <NA>        <NA>
2355          <NA>        <NA>        <NA>
2356          <NA>        <NA>        <NA>
2357          <NA>        <NA>        <NA>
2358          <NA>        <NA>        <NA>
2359          <NA>        <NA>        <NA>
2360          <NA>        <NA>        <NA>
2361          <NA>        <NA>        <NA>
2362          <NA>        <NA>        <NA>
2363          <NA>        <NA>        <NA>
2364          <NA>        <NA>        <NA>
2365          <NA>        <NA>        <NA>
2366          <NA>        <NA>        <NA>
2367          <NA>        <NA>        <NA>
2368          <NA>        <NA>        <NA>
2369          <NA>        <NA>        <NA>
2370          <NA>        <NA>        <NA>
2371          <NA>        <NA>        <NA>
2372          <NA>        <NA>        <NA>
2373          <NA>        <NA>        <NA>
2374          <NA>        <NA>        <NA>
2375          <NA>        <NA>        <NA>
2376          <NA>        <NA>        <NA>
2377          <NA>        <NA>        <NA>
2378          <NA>        <NA>        <NA>
2379          <NA>        <NA>        <NA>
2380          <NA>        <NA>        <NA>
2381          <NA>        <NA>        <NA>
2382          <NA>        <NA>        <NA>
2383          <NA>        <NA>        <NA>
2384          <NA>        <NA>        <NA>
2385          <NA>        <NA>        <NA>
2386          <NA>        <NA>        <NA>
2387          <NA>        <NA>        <NA>
2388          <NA>        <NA>        <NA>
2389          <NA>        <NA>        <NA>
2390          pink       white        <NA>
2391          pink       white        <NA>
2392          pink       white        <NA>
2393          pink       white        <NA>
2394          pink       white        <NA>
2395          pink       white        <NA>
2396          pink       white        <NA>
2397          <NA>        <NA>        <NA>
2398          <NA>        <NA>        <NA>
2399          <NA>        <NA>        <NA>
2400          <NA>        <NA>        <NA>
2401          <NA>        <NA>        <NA>
2402          <NA>        <NA>        <NA>
2403          <NA>        <NA>        <NA>
2404          <NA>        <NA>        <NA>
2405          <NA>        <NA>        <NA>
2406          <NA>        <NA>        <NA>
2407          <NA>        <NA>        <NA>
2408          <NA>        <NA>        <NA>
2409          <NA>        <NA>        <NA>
2410          <NA>        <NA>        <NA>
2411          pink       white        <NA>
2412          pink       white        <NA>
2413          pink       white        <NA>
2414          pink       white        <NA>
2415          pink       white        <NA>
2416          pink       white        <NA>
2417          pink       white        <NA>
2418          pink       white        <NA>
2419          pink       white        <NA>
2420          pink       white        <NA>
2421          pink       white        <NA>
2422          pink       white        <NA>
2423          pink       white        <NA>
2424          pink       white        <NA>
2425          pink       white        <NA>
2426          pink       white        <NA>
2427          pink       white        <NA>
2428          pink       white        <NA>
2429          pink       white        <NA>
2430          pink       white        <NA>
2431          pink       white        <NA>
2432          <NA>        <NA>        <NA>
2433          <NA>        <NA>        <NA>
2434          <NA>        <NA>        <NA>
2435          <NA>        <NA>        <NA>
2436         pink,       white        <NA>
2437         pink,       white        <NA>
2438         pink,       white        <NA>
2439         pink,       white        <NA>
2440         pink,       white        <NA>
2441         pink,       white        <NA>
2442         pink,       white        <NA>
2443          pink        <NA>        <NA>
2444          pink        <NA>        <NA>
2445          pink        <NA>        <NA>
2446          pink        <NA>        <NA>
2447          pink        <NA>        <NA>
2448          pink        <NA>        <NA>
2449          pink        <NA>        <NA>
2450          pink        <NA>        <NA>
2451          pink        <NA>        <NA>
2452          pink        <NA>        <NA>
2453          pink        <NA>        <NA>
2454          pink        <NA>        <NA>
2455          pink        <NA>        <NA>
2456          pink        <NA>        <NA>
2457          pink        <NA>        <NA>
2458          pink        <NA>        <NA>
2459          pink        <NA>        <NA>
2460          pink        <NA>        <NA>
2461          pink        <NA>        <NA>
2462          <NA>        <NA>        <NA>
2463          <NA>        <NA>        <NA>
2464          <NA>        <NA>        <NA>
2465          <NA>        <NA>        <NA>
2466          <NA>        <NA>        <NA>
2467          <NA>        <NA>        <NA>
2468          <NA>        <NA>        <NA>
2469          <NA>        <NA>        <NA>
2470          <NA>        <NA>        <NA>
2471          <NA>        <NA>        <NA>
2472          <NA>        <NA>        <NA>
2473          <NA>        <NA>        <NA>
2474          <NA>        <NA>        <NA>
2475          <NA>        <NA>        <NA>
2476          <NA>        <NA>        <NA>
2477          <NA>        <NA>        <NA>
2478          <NA>        <NA>        <NA>
2479          <NA>        <NA>        <NA>
2480          <NA>        <NA>        <NA>
2481          <NA>        <NA>        <NA>
2482          <NA>        <NA>        <NA>
2483          <NA>        <NA>        <NA>
2484          <NA>        <NA>        <NA>
2485          <NA>        <NA>        <NA>
2486          <NA>        <NA>        <NA>
2487          <NA>        <NA>        <NA>
2488          <NA>        <NA>        <NA>
2489          pink        <NA>        <NA>
2490          pink        <NA>        <NA>
2491          pink        <NA>        <NA>
2492          pink        <NA>        <NA>
2493          <NA>        <NA>        <NA>
2494          <NA>        <NA>        <NA>
2495          <NA>        <NA>        <NA>
2496          <NA>        <NA>        <NA>
2497          <NA>        <NA>        <NA>
2498          <NA>        <NA>        <NA>
2499          <NA>        <NA>        <NA>
2500          <NA>        <NA>        <NA>
2501          <NA>        <NA>        <NA>
2502          <NA>        <NA>        <NA>
2503          <NA>        <NA>        <NA>
2504          <NA>        <NA>        <NA>
2505          <NA>        <NA>        <NA>
2506          <NA>        <NA>        <NA>
2507          <NA>        <NA>        <NA>
2508          <NA>        <NA>        <NA>
2509          <NA>        <NA>        <NA>
2510          <NA>        <NA>        <NA>
2511          <NA>        <NA>        <NA>
2512          <NA>        <NA>        <NA>
2513          <NA>        <NA>        <NA>
2514          <NA>        <NA>        <NA>
2515          <NA>        <NA>        <NA>
2516          <NA>        <NA>        <NA>
2517          <NA>        <NA>        <NA>
2518          <NA>        <NA>        <NA>
2519          <NA>        <NA>        <NA>
2520          <NA>        <NA>        <NA>
2521          <NA>        <NA>        <NA>
2522          <NA>        <NA>        <NA>
2523          <NA>        <NA>        <NA>
2524          <NA>        <NA>        <NA>
2525          <NA>        <NA>        <NA>
2526          <NA>        <NA>        <NA>
2527          <NA>        <NA>        <NA>
2528          <NA>        <NA>        <NA>
2529          <NA>        <NA>        <NA>
2530          <NA>        <NA>        <NA>
2531          <NA>        <NA>        <NA>
2532          <NA>        <NA>        <NA>
2533          <NA>        <NA>        <NA>
2534          <NA>        <NA>        <NA>
2535          <NA>        <NA>        <NA>
2536          <NA>        <NA>        <NA>
2537          <NA>        <NA>        <NA>
2538          <NA>        <NA>        <NA>
2539          <NA>        <NA>        <NA>
2540          <NA>        <NA>        <NA>
2541          <NA>        <NA>        <NA>
2542          <NA>        <NA>        <NA>
2543          <NA>        <NA>        <NA>
2544          <NA>        <NA>        <NA>
2545          <NA>        <NA>        <NA>
2546         white        <NA>        <NA>
2547         white        <NA>        <NA>
2548         white        <NA>        <NA>
2549         white        <NA>        <NA>
2550         white        <NA>        <NA>
2551         white        <NA>        <NA>
2552         white        <NA>        <NA>
2553         white        <NA>        <NA>
2554         white        <NA>        <NA>
2555         white        <NA>        <NA>
2556         white        <NA>        <NA>
2557         white        <NA>        <NA>
2558         white        <NA>        <NA>
2559         white        <NA>        <NA>
2560         white        <NA>        <NA>
2561         white        <NA>        <NA>
2562         white        <NA>        <NA>
2563         white        <NA>        <NA>
2564         white        <NA>        <NA>
2565         white        <NA>        <NA>
2566         white        <NA>        <NA>
2567          <NA>        <NA>        <NA>
2568          <NA>        <NA>        <NA>
2569          <NA>        <NA>        <NA>
2570          <NA>        <NA>        <NA>
2571          <NA>        <NA>        <NA>
2572          <NA>        <NA>        <NA>
2573          <NA>        <NA>        <NA>
2574          <NA>        <NA>        <NA>
2575          <NA>        <NA>        <NA>
2576          <NA>        <NA>        <NA>
2577          <NA>        <NA>        <NA>
2578          <NA>        <NA>        <NA>
2579          <NA>        <NA>        <NA>
2580          <NA>        <NA>        <NA>
2581          <NA>        <NA>        <NA>
2582          <NA>        <NA>        <NA>
2583          <NA>        <NA>        <NA>
2584          <NA>        <NA>        <NA>
2585          <NA>        <NA>        <NA>
2586          <NA>        <NA>        <NA>
2587          <NA>        <NA>        <NA>
2588          <NA>        <NA>        <NA>
2589          <NA>        <NA>        <NA>
2590          <NA>        <NA>        <NA>
2591          <NA>        <NA>        <NA>
2592          <NA>        <NA>        <NA>
2593          <NA>        <NA>        <NA>
2594          <NA>        <NA>        <NA>
2595          <NA>        <NA>        <NA>
2596          <NA>        <NA>        <NA>
2597          <NA>        <NA>        <NA>
2598          <NA>        <NA>        <NA>
2599          <NA>        <NA>        <NA>
2600          <NA>        <NA>        <NA>
2601          <NA>        <NA>        <NA>
2602          <NA>        <NA>        <NA>
2603          <NA>        <NA>        <NA>
2604          <NA>        <NA>        <NA>
2605          <NA>        <NA>        <NA>
2606          <NA>        <NA>        <NA>
2607          <NA>        <NA>        <NA>
2608          <NA>        <NA>        <NA>
2609          <NA>        <NA>        <NA>
2610          <NA>        <NA>        <NA>
2611          <NA>        <NA>        <NA>
2612          <NA>        <NA>        <NA>
2613          <NA>        <NA>        <NA>
2614          <NA>        <NA>        <NA>
2615          <NA>        <NA>        <NA>
2616          <NA>        <NA>        <NA>
2617          <NA>        <NA>        <NA>
2618          <NA>        <NA>        <NA>
2619          <NA>        <NA>        <NA>
2620          <NA>        <NA>        <NA>
2621          <NA>        <NA>        <NA>
2622          <NA>        <NA>        <NA>
2623          <NA>        <NA>        <NA>
2624          <NA>        <NA>        <NA>
2625          <NA>        <NA>        <NA>
2626          <NA>        <NA>        <NA>
2627          <NA>        <NA>        <NA>
2628          <NA>        <NA>        <NA>
2629          <NA>        <NA>        <NA>
2630          <NA>        <NA>        <NA>
2631          <NA>        <NA>        <NA>
2632          <NA>        <NA>        <NA>
2633          <NA>        <NA>        <NA>
2634          <NA>        <NA>        <NA>
2635          <NA>        <NA>        <NA>
2636          <NA>        <NA>        <NA>
2637          <NA>        <NA>        <NA>
2638          <NA>        <NA>        <NA>
2639          <NA>        <NA>        <NA>
2640          <NA>        <NA>        <NA>
2641          <NA>        <NA>        <NA>
2642          <NA>        <NA>        <NA>
2643          <NA>        <NA>        <NA>
2644          <NA>        <NA>        <NA>
2645          <NA>        <NA>        <NA>
2646          <NA>        <NA>        <NA>
2647          <NA>        <NA>        <NA>
2648          <NA>        <NA>        <NA>
2649          <NA>        <NA>        <NA>
2650          <NA>        <NA>        <NA>
2651          <NA>        <NA>        <NA>
2652          <NA>        <NA>        <NA>
2653          <NA>        <NA>        <NA>
2654          <NA>        <NA>        <NA>
2655          <NA>        <NA>        <NA>
2656          <NA>        <NA>        <NA>
2657          <NA>        <NA>        <NA>
2658          <NA>        <NA>        <NA>
2659          <NA>        <NA>        <NA>
2660          <NA>        <NA>        <NA>
2661          <NA>        <NA>        <NA>
2662          <NA>        <NA>        <NA>
2663          <NA>        <NA>        <NA>
2664          <NA>        <NA>        <NA>
2665          <NA>        <NA>        <NA>
2666          <NA>        <NA>        <NA>
2667          <NA>        <NA>        <NA>
2668          <NA>        <NA>        <NA>
2669          <NA>        <NA>        <NA>
2670          <NA>        <NA>        <NA>
2671          <NA>        <NA>        <NA>
2672          <NA>        <NA>        <NA>
2673          <NA>        <NA>        <NA>
2674          <NA>        <NA>        <NA>
2675          <NA>        <NA>        <NA>
2676          <NA>        <NA>        <NA>
2677          <NA>        <NA>        <NA>
2678          <NA>        <NA>        <NA>
2679          <NA>        <NA>        <NA>
2680          <NA>        <NA>        <NA>
2681          <NA>        <NA>        <NA>
2682          <NA>        <NA>        <NA>
2683          <NA>        <NA>        <NA>
2684          <NA>        <NA>        <NA>
2685          <NA>        <NA>        <NA>
2686          <NA>        <NA>        <NA>
2687          <NA>        <NA>        <NA>
2688          <NA>        <NA>        <NA>
2689          <NA>        <NA>        <NA>
                                  color_source pollinator_1  pollinator_2
1                              Hsu,  Hall 2003  hummingbird          <NA>
2                              Hsu,  Hall 2003  hummingbird          <NA>
3                              Hsu,  Hall 2003  hummingbird          <NA>
4                              Hsu,  Hall 2003  hummingbird          <NA>
5                              Hsu,  Hall 2003  hummingbird          <NA>
6                                  Porter 1998  hummingbird          <NA>
7                                  Porter 1998  hummingbird          <NA>
8                                  Porter 1998  hummingbird          <NA>
9                                  Porter 1998  hummingbird          <NA>
10                                 Porter 1998         <NA>          <NA>
11                                 Porter 1998         <NA>          <NA>
12                                 Porter 1998         <NA>          <NA>
13                                 Porter 1998         <NA>          <NA>
14                                 Porter 1998         <NA>          <NA>
15                                 Porter 1998         <NA>          <NA>
16                                 Porter 1998         <NA>          <NA>
17                                 Porter 1998         <NA>          <NA>
18                                 Porter 1998         <NA>          <NA>
19                                 Porter 1998         <NA>          <NA>
20                                 Porter 1998         <NA>          <NA>
21                                 Porter 1998         <NA>          <NA>
22                                 Porter 1998         <NA>          <NA>
23                                 Porter 1998         <NA>          <NA>
24                                 Porter 1998         <NA>          <NA>
25                                 Porter 1998         <NA>          <NA>
26                                 Porter 1998         <NA>          <NA>
27                                 Porter 1998         <NA>          <NA>
28                                 Porter 1998         <NA>          <NA>
29                                 Porter 1998         <NA>          <NA>
30                                 Porter 1998         <NA>          <NA>
31                                 Porter 1998         <NA>          <NA>
32                                 Porter 1998         <NA>          <NA>
33                                 Porter 1998         <NA>          <NA>
34                                 Porter 1998         <NA>          <NA>
35                                 Porter 1998         <NA>          <NA>
36                                 Porter 1998         <NA>          <NA>
37                                 Porter 1998         <NA>          <NA>
38                                 Porter 1998         <NA>          <NA>
39                                 Porter 1998         <NA>          <NA>
40                                 Porter 1998         <NA>          <NA>
41                                 Porter 1998         <NA>          <NA>
42                                 Porter 1998         <NA>          <NA>
43                                 Porter 1998         <NA>          <NA>
44                                 Porter 1998         <NA>          <NA>
45                                 Porter 1998         <NA>          <NA>
46                                 Porter 1998         <NA>          <NA>
47                                 Porter 1998         <NA>          <NA>
48                                 Porter 1998         <NA>          <NA>
49                                 Porter 1998         <NA>          <NA>
50                                 Porter 1998         <NA>          <NA>
51                                 Porter 1998         <NA>          <NA>
52                                 Porter 1998         <NA>          <NA>
53                                 Porter 1998         <NA>          <NA>
54                                 Porter 1998         <NA>          <NA>
55                                 Porter 1998         <NA>          <NA>
56                                 Porter 1998         <NA>          <NA>
57                                 Porter 1998         <NA>          <NA>
58                                 Porter 1998         <NA>          <NA>
59                                 Porter 1998         <NA>          <NA>
60                                 Porter 1998         <NA>          <NA>
61                                 Porter 1998         <NA>          <NA>
62                                 Porter 1998         <NA>          <NA>
63                                 Porter 1998         <NA>          <NA>
64                                 Porter 1998         <NA>          <NA>
65                                 Porter 1998         <NA>          <NA>
66                                 Porter 1998         <NA>          <NA>
67                                 Porter 1998         <NA>          <NA>
68                                 Porter 1998         <NA>          <NA>
69                                 Porter 1998         <NA>          <NA>
70                                 Porter 1998         <NA>          <NA>
71                                 Porter 1998         <NA>          <NA>
72                                 Porter 1998         <NA>          <NA>
73                                 Porter 1998         <NA>          <NA>
74                                 Porter 1998         <NA>          <NA>
75                                 Porter 1998         <NA>          <NA>
76                                 Porter 1998         <NA>          <NA>
77                                 Porter 1998         <NA>          <NA>
78                                 Porter 1998         <NA>          <NA>
79                                 Porter 1998         <NA>          <NA>
80                                 Porter 1998         <NA>          <NA>
81                                 Porter 1998         <NA>          <NA>
82                               Jepson Online         <NA>          <NA>
83                               Jepson Online         <NA>          <NA>
84                               Jepson Online         <NA>          <NA>
85                               Jepson Online         <NA>          <NA>
86                               Jepson Online         <NA>          <NA>
87                               Jepson Online         <NA>          <NA>
88                               Jepson Online         <NA>          <NA>
89                               Jepson Online         <NA>          <NA>
90                               Jepson Online         <NA>          <NA>
91                               Jepson Online         <NA>          <NA>
92                               Jepson Online         <NA>          <NA>
93                               Jepson Online         <NA>          <NA>
94                               Jepson Online         <NA>          <NA>
95                                 Porter 1998         <NA>          <NA>
96                                 Porter 1998         <NA>          <NA>
97                                 Porter 1998         <NA>          <NA>
98                                 Porter 1998         <NA>          <NA>
99                                 Porter 1998         <NA>          <NA>
100                                Porter 1998         <NA>          <NA>
101                                Porter 1998         <NA>          <NA>
102                                Porter 1998         <NA>          <NA>
103                                Porter 1998         <NA>          <NA>
104                                Porter 1998         <NA>          <NA>
105                                Porter 1998         <NA>          <NA>
106                                Porter 1998         <NA>          <NA>
107                                Porter 1998         <NA>          <NA>
108                                Porter 1998          bee          <NA>
109                                Porter 1998          bee          <NA>
110                                Porter 1998          bee          <NA>
111                                Porter 1998          bee          <NA>
112                                Porter 1998          bee          <NA>
113                                Porter 1998          bee          <NA>
114                                Porter 1998          bee          <NA>
115                                Porter 1998          bee          <NA>
116                                Porter 1998          bee          <NA>
117                                Porter 1998          bee          <NA>
118                                Porter 1998         <NA>          <NA>
119                                Porter 1998         <NA>          <NA>
120                                Porter 1998         <NA>          <NA>
121                                Porter 1998         <NA>          <NA>
122                                Porter 1998         <NA>          <NA>
123                                Porter 1998         <NA>          <NA>
124                                Porter 1998         <NA>          <NA>
125                                Porter 1998         <NA>          <NA>
126                                Porter 1998         <NA>          <NA>
127                                Porter 1998         <NA>          <NA>
128                                Porter 1998         <NA>          <NA>
129                                Porter 1998         <NA>          <NA>
130                                Porter 1998         <NA>          <NA>
131                                Porter 1998         <NA>          <NA>
132                                Porter 1998         <NA>          <NA>
133                                Porter 1998         <NA>          <NA>
134                                Porter 1998         <NA>          <NA>
135                                Porter 1998         <NA>          <NA>
136                                Porter 1998         <NA>          <NA>
137                                Porter 1998         <NA>          <NA>
138                                Porter 1998         <NA>          <NA>
139                                Porter 1998         <NA>          <NA>
140                                Porter 1998         <NA>          <NA>
141                                Porter 1998         <NA>          <NA>
142                                Porter 1998  hummingbird          <NA>
143                                Porter 1998  hummingbird          <NA>
144                                Porter 1998  hummingbird          <NA>
145                                Porter 1998  hummingbird          <NA>
146                                Porter 1998  hummingbird          <NA>
147                                Porter 1998  hummingbird          <NA>
148                                Porter 1998  hummingbird          <NA>
149                                Porter 1998  hummingbird          <NA>
150                                Porter 1998  hummingbird          <NA>
151                                Porter 1998  hummingbird          <NA>
152                                Porter 1998  hummingbird          <NA>
153                                Porter 1998  hummingbird          <NA>
154                                Porter 1998  hummingbird          <NA>
155                                Porter 1998  hummingbird          <NA>
156                                Porter 1998  hummingbird          <NA>
157                                Porter 1998  hummingbird          <NA>
158                                Porter 1998  hummingbird          <NA>
159                                Porter 1998  hummingbird          <NA>
160                                Porter 1998  hummingbird          <NA>
161                                Porter 1998         <NA>          <NA>
162                                Porter 1998         <NA>          <NA>
163                                Porter 1998         <NA>          <NA>
164                                Porter 1998         <NA>          <NA>
165                                Porter 1998         <NA>          <NA>
166                                Porter 1998         <NA>          <NA>
167                                Porter 1998         <NA>          <NA>
168                                Porter 1998         <NA>          <NA>
169                                Porter 1998         <NA>          <NA>
170                                Porter 1998         <NA>          <NA>
171                                Porter 1998         <NA>          <NA>
172                                Porter 1998         <NA>          <NA>
173                                Porter 1998         <NA>          <NA>
174                              Jepson Online      bee-fly          <NA>
175                              Jepson Online      bee-fly          <NA>
176                              Jepson Online      bee-fly          <NA>
177                              Jepson Online      bee-fly          <NA>
178                              Jepson Online         <NA>          <NA>
179                              Jepson Online         <NA>          <NA>
180                              Jepson Online         <NA>          <NA>
181                              Jepson Online         <NA>          <NA>
182                              Jepson Online         <NA>          <NA>
183                              Jepson Online         <NA>          <NA>
184                              Jepson Online         <NA>          <NA>
185                              Jepson Online         <NA>          <NA>
186                              Jepson Online         <NA>          <NA>
187                              Jepson Online         <NA>          <NA>
188                              Jepson Online         <NA>          <NA>
189                              Jepson Online         <NA>          <NA>
190                              Jepson Online         <NA>          <NA>
191                              Jepson Online         <NA>          <NA>
192                              Jepson Online         <NA>          <NA>
193                              Jepson Online         <NA>          <NA>
194                              Jepson Online         <NA>          <NA>
195                              Jepson Online         <NA>          <NA>
196                              Jepson Online         <NA>          <NA>
197                              Jepson Online         <NA>          <NA>
198                              Jepson Online         <NA>          <NA>
199                              Jepson Online         <NA>          <NA>
200                              Jepson Online         <NA>          <NA>
201                              Jepson Online         <NA>          <NA>
202                              Jepson Online         <NA>          <NA>
203                              Jepson Online         <NA>          <NA>
204                              Jepson Online         <NA>          <NA>
205                              Jepson Online         <NA>          <NA>
206                              Jepson Online         <NA>          <NA>
207                              Jepson Online         <NA>          <NA>
208                              Jepson Online          bee       bee-fly
209                              Jepson Online          bee       bee-fly
210                              Jepson Online          bee       bee-fly
211                              Jepson Online          bee       bee-fly
212                              Jepson Online          bee       bee-fly
213                              Jepson Online          bee       bee-fly
214                              Jepson Online          bee       bee-fly
215                              Jepson Online         <NA>          <NA>
216                              Jepson Online         <NA>          <NA>
217                              Jepson Online         <NA>          <NA>
218                              Jepson Online         <NA>          <NA>
219                              Jepson Online         <NA>          <NA>
220                              Jepson Online         <NA>          <NA>
221                              Jepson Online         <NA>          <NA>
222                              Jepson Online         <NA>          <NA>
223                              Jepson Online         <NA>          <NA>
224                              Jepson Online         <NA>          <NA>
225                              Jepson Online         <NA>          <NA>
226                              Jepson Online         <NA>          <NA>
227                              Jepson Online         <NA>          <NA>
228                              Jepson Online         <NA>          <NA>
229                              Jepson Online         <NA>          <NA>
230                              Jepson Online         <NA>          <NA>
231                              Jepson Online         <NA>          <NA>
232                              Jepson Online         <NA>          <NA>
233                              Jepson Online         <NA>          <NA>
234                              Jepson Online         <NA>          <NA>
235                         Grant,  Grant 1965          bee          <NA>
236                         Grant,  Grant 1965          bee          <NA>
237                         Grant,  Grant 1965          bee          <NA>
238                         Grant,  Grant 1965          bee          <NA>
239                         Grant,  Grant 1965          bee          <NA>
240                         Grant,  Grant 1965          bee          <NA>
241                         Grant,  Grant 1965          bee          <NA>
242                      Porter,  Johnson 2000         <NA>          <NA>
243                      Porter,  Johnson 2000         <NA>          <NA>
244                      Porter,  Johnson 2000         <NA>          <NA>
245                      Porter,  Johnson 2000         <NA>          <NA>
246                         Grant,  Grant 1965  hummingbird          <NA>
247                         Grant,  Grant 1965  hummingbird          <NA>
248                         Grant,  Grant 1965  hummingbird          <NA>
249                         Grant,  Grant 1965  hummingbird          <NA>
250                         Grant,  Grant 1965  hummingbird          <NA>
251                         Grant,  Grant 1965  hummingbird          <NA>
252                         Grant,  Grant 1965  hummingbird          <NA>
253                         Grant,  Grant 1965  hummingbird          <NA>
254                         Grant,  Grant 1965  hummingbird          <NA>
255                         Grant,  Grant 1965  hummingbird          <NA>
256                         Grant,  Grant 1965  hummingbird          <NA>
257                         Grant,  Grant 1965  hummingbird          <NA>
258                         Grant,  Grant 1965  hummingbird          <NA>
259                         Grant,  Grant 1965  hummingbird          <NA>
260                         Grant,  Grant 1965  hummingbird          <NA>
261                         Grant,  Grant 1965  hummingbird          <NA>
262                         Grant,  Grant 1965  hummingbird          <NA>
263     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
264     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
265     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
266     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
267                      Porter,  Prather 1998         <NA>          <NA>
268                      Porter,  Prather 1998         <NA>          <NA>
269                      Porter,  Prather 1998         <NA>          <NA>
270                      Porter,  Prather 1998         <NA>          <NA>
271     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
272     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
273     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
274     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
275     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
276     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
277     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
278     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
279     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
280     Polemoniaceae Tree of Life Web Project         <NA>          <NA>
281                         Grant,  Grant 1965         <NA>          <NA>
282                         Grant,  Grant 1965         <NA>          <NA>
283                         Grant,  Grant 1965         <NA>          <NA>
284                         Grant,  Grant 1965         <NA>          <NA>
285                         Grant,  Grant 1965         <NA>          <NA>
286                         Grant,  Grant 1965         <NA>          <NA>
287                         Grant,  Grant 1965         <NA>          <NA>
288                         Grant,  Grant 1965         <NA>          <NA>
289                         Grant,  Grant 1965         <NA>          <NA>
290                         Grant,  Grant 1965         <NA>          <NA>
291                         Grant,  Grant 1965  hummingbird      hawkmoth
292                         Grant,  Grant 1965  hummingbird      hawkmoth
293                         Grant,  Grant 1965  hummingbird      hawkmoth
294                         Grant,  Grant 1965  hummingbird      hawkmoth
295                         Grant,  Grant 1965  hummingbird      hawkmoth
296                         Grant,  Grant 1965  hummingbird      hawkmoth
297                         Grant,  Grant 1965  hummingbird      hawkmoth
298                         Grant,  Grant 1965  hummingbird      hawkmoth
299                         Grant,  Grant 1965  hummingbird      hawkmoth
300                         Grant,  Grant 1965  hummingbird      hawkmoth
301                         Grant,  Grant 1965  hummingbird      hawkmoth
302                         Grant,  Grant 1965  hummingbird      hawkmoth
303                         Grant,  Grant 1965  hummingbird      hawkmoth
304                         Grant,  Grant 1965         <NA>          <NA>
305                         Grant,  Grant 1965         <NA>          <NA>
306                         Grant,  Grant 1965         <NA>          <NA>
307                         Grant,  Grant 1965         <NA>          <NA>
308                              Prather 1999b          bat          <NA>
309                              Prather 1999b          bat          <NA>
310                              Prather 1999b         <NA>          <NA>
311                              Prather 1999b         <NA>          <NA>
312                              Prather 1999b         <NA>          <NA>
313                              Prather 1999b         <NA>          <NA>
314                              Prather 1999b         <NA>          <NA>
315                              Prather 1999b         <NA>          <NA>
316                              Prather 1999b         <NA>          <NA>
317                              Prather 1999b         <NA>          <NA>
318                              Prather 1999b         <NA>          <NA>
319                              Prather 1999b  hummingbird          <NA>
320                              Prather 1999b  hummingbird          <NA>
321                              Prather 1999b         <NA>          <NA>
322                              Prather 1999b         <NA>          <NA>
323                              Prather 1999b         <NA>          <NA>
324                              Prather 1999b         <NA>          <NA>
325                              Prather 1999b         <NA>          <NA>
326                              Prather 1999b         <NA>          <NA>
327                              Prather 1999b         <NA>          <NA>
328                              Prather 1999b         <NA>          <NA>
329                              Prather 1999b         <NA>          <NA>
330                              Prather 1999b         <NA>          <NA>
331                              Prather 1999b         <NA>          <NA>
332                              Prather 1999b         <NA>          <NA>
333                              Prather 1999b         <NA>          <NA>
334                              Prather 1999b         <NA>          <NA>
335                              Prather 1999b         <NA>          <NA>
336                              Prather 1999b         <NA>          <NA>
337                              Prather 1999b         <NA>          <NA>
338                              Prather 1999b         <NA>          <NA>
339                              Prather 1999b         <NA>          <NA>
340                              Prather 1999b         <NA>          <NA>
341                              Prather 1999b         <NA>          <NA>
342                              Prather 1999b  hummingbird          <NA>
343                              Prather 1999b  hummingbird          <NA>
344                              Prather 1999b  hummingbird          <NA>
345                              Prather 1999b  hummingbird          <NA>
346                              Prather 1999b  hummingbird          <NA>
347                              Prather 1999b         <NA>          <NA>
348                              Prather 1999b         <NA>          <NA>
349                              Prather 1999b         <NA>          <NA>
350                              Prather 1999b         <NA>          <NA>
351                              Prather 1999b         <NA>          <NA>
352                              Prather 1999b         <NA>          <NA>
353                              Prather 1999b         <NA>          <NA>
354                              Prather 1999b     hawkmoth          <NA>
355                              Prather 1999b     hawkmoth          <NA>
356                              Prather 1999b     hawkmoth          <NA>
357                              Prather 1999b         <NA>          <NA>
358                              Prather 1999b         <NA>          <NA>
359                              Prather 1999b         <NA>          <NA>
360                              Prather 1999b         <NA>          <NA>
361                              Prather 1999b          bat          <NA>
362                              Prather 1999b          bat          <NA>
363                              Prather 1999b          bat          <NA>
364                              Prather 1999b          bat          <NA>
365                              Prather 1999b          bat          <NA>
366                              Prather 1999b          bat          <NA>
367                              Prather 1999b          bat          <NA>
368                              Prather 1999b         <NA>          <NA>
369                              Prather 1999b         <NA>          <NA>
370                              Prather 1999b         <NA>          <NA>
371                              Prather 1999b          bat          <NA>
372                              Prather 1999b          bat          <NA>
373                              Prather 1999b          bat          <NA>
374                              Prather 1999b          bat          <NA>
375                  Chileflora Plant Database         <NA>          <NA>
376                  Chileflora Plant Database         <NA>          <NA>
377                  Chileflora Plant Database         <NA>          <NA>
378                  Chileflora Plant Database         <NA>          <NA>
379                  Chileflora Plant Database         <NA>          <NA>
380                  Chileflora Plant Database         <NA>          <NA>
381                  Chileflora Plant Database         <NA>          <NA>
382                  Chileflora Plant Database         <NA>          <NA>
383                  Chileflora Plant Database         <NA>          <NA>
384                  Chileflora Plant Database         <NA>          <NA>
385             Flora of the Pacific Northwest         <NA>          <NA>
386             Flora of the Pacific Northwest         <NA>          <NA>
387             Flora of the Pacific Northwest         <NA>          <NA>
388             Flora of the Pacific Northwest         <NA>          <NA>
389             Flora of the Pacific Northwest         <NA>          <NA>
390             Flora of the Pacific Northwest         <NA>          <NA>
391             Flora of the Pacific Northwest         <NA>          <NA>
392             Flora of the Pacific Northwest         <NA>          <NA>
393             Flora of the Pacific Northwest         <NA>          <NA>
394             Flora of the Pacific Northwest         <NA>          <NA>
395             Flora of the Pacific Northwest         <NA>          <NA>
396                              Jepson Online         <NA>          <NA>
397                              Jepson Online         <NA>          <NA>
398                              Jepson Online         <NA>          <NA>
399                              Jepson Online         <NA>          <NA>
400                              Milliken 2010          bee          <NA>
401                              Milliken 2010          bee          <NA>
402                              Milliken 2010          bee          <NA>
403                              Milliken 2010          bee          <NA>
404                              Milliken 2010          bee          <NA>
405                              Milliken 2010          bee          <NA>
406                              Milliken 2010          bee          <NA>
407                              Milliken 2010          bee          <NA>
408                              Milliken 2010          bee          <NA>
409                              Milliken 2010          bee          <NA>
410                              Milliken 2010          bee          <NA>
411                              Milliken 2010          bee          <NA>
412                              Milliken 2010          bee          <NA>
413                              Milliken 2010          bee          <NA>
414                              Milliken 2010          bee          <NA>
415                              Milliken 2010          bee          <NA>
416                              Milliken 2010          bee          <NA>
417                              Milliken 2010          bee          <NA>
418                              Milliken 2010          bee          <NA>
419                              Milliken 2010          bee          <NA>
420                              Milliken 2010          bee          <NA>
421                              Milliken 2010          bee          <NA>
422                              Milliken 2010          bee          <NA>
423                              Milliken 2010          bee          <NA>
424                              Milliken 2010          bee          <NA>
425                              Milliken 2010          bee          <NA>
426                              Jepson Online         <NA>          <NA>
427                              Jepson Online         <NA>          <NA>
428                              Jepson Online         <NA>          <NA>
429                              Jepson Online         <NA>          <NA>
430                              Jepson Online         <NA>          <NA>
431                              Jepson Online         <NA>          <NA>
432                              Jepson Online         <NA>          <NA>
433                              Jepson Online         <NA>          <NA>
434                              Jepson Online         <NA>          <NA>
435                              Jepson Online         <NA>          <NA>
436                              Jepson Online         <NA>          <NA>
437                              Jepson Online         <NA>          <NA>
438                              Jepson Online         <NA>          <NA>
439                              Jepson Online         <NA>          <NA>
440                              Jepson Online         <NA>          <NA>
441                              Jepson Online         <NA>          <NA>
442                              Jepson Online         <NA>          <NA>
443                              Jepson Online         <NA>          <NA>
444                              Jepson Online         <NA>          <NA>
445                              Jepson Online         <NA>          <NA>
446                              Jepson Online         <NA>          <NA>
447                              Jepson Online         <NA>          <NA>
448                              Jepson Online         <NA>          <NA>
449                              Jepson Online         <NA>          <NA>
450                              Jepson Online         <NA>          <NA>
451                              Jepson Online         <NA>          <NA>
452                              Jepson Online         <NA>          <NA>
453                              Jepson Online         <NA>          <NA>
454                              Jepson Online         <NA>          <NA>
455                              Jepson Online         <NA>          <NA>
456                              Jepson Online         <NA>          <NA>
457                              Jepson Online         <NA>          <NA>
458                              Jepson Online         <NA>          <NA>
459                              Jepson Online         <NA>          <NA>
460                              Jepson Online         <NA>          <NA>
461                              Jepson Online         <NA>          <NA>
462                              Jepson Online         <NA>          <NA>
463                              Jepson Online         <NA>          <NA>
464                              Jepson Online         <NA>          <NA>
465                              Milliken 2010         <NA>          <NA>
466                              Milliken 2010         <NA>          <NA>
467                              Milliken 2010         <NA>          <NA>
468                              Milliken 2010         <NA>          <NA>
469                              Milliken 2010         <NA>          <NA>
470                              Milliken 2010         <NA>          <NA>
471                              Milliken 2010         <NA>          <NA>
472                              Milliken 2010         <NA>          <NA>
473                              Milliken 2010         <NA>          <NA>
474                              Milliken 2010         <NA>          <NA>
475                              Milliken 2010         <NA>          <NA>
476                              Milliken 2010         <NA>          <NA>
477                              Milliken 2010         <NA>          <NA>
478                              Milliken 2010         <NA>          <NA>
479                              Milliken 2010         <NA>          <NA>
480                              Milliken 2010         <NA>          <NA>
481                              Milliken 2010         <NA>          <NA>
482                              Milliken 2010         <NA>          <NA>
483                              Milliken 2010         <NA>          <NA>
484                              Milliken 2010         <NA>          <NA>
485                              Milliken 2010         <NA>          <NA>
486                              Milliken 2010         <NA>          <NA>
487                              Milliken 2010         <NA>          <NA>
488                              Milliken 2010         <NA>          <NA>
489                              Milliken 2010         <NA>          <NA>
490                              Milliken 2010         <NA>          <NA>
491                              Milliken 2010         <NA>          <NA>
492                              Milliken 2010         <NA>          <NA>
493                              Milliken 2010         <NA>          <NA>
494                              Milliken 2010         <NA>          <NA>
495                              Milliken 2010         <NA>          <NA>
496                              Milliken 2010         <NA>          <NA>
497                              Milliken 2010         <NA>          <NA>
498                              Milliken 2010         <NA>          <NA>
499                              Milliken 2010         <NA>          <NA>
500                              Milliken 2010         <NA>          <NA>
501                              Milliken 2010         <NA>          <NA>
502                              Milliken 2010         <NA>          <NA>
503                              Milliken 2010         <NA>          <NA>
504                              Milliken 2010         <NA>          <NA>
505                              Milliken 2010      bee-fly          <NA>
506                              Milliken 2010      bee-fly          <NA>
507                              Milliken 2010      bee-fly          <NA>
508                              Milliken 2010      bee-fly          <NA>
509                              Milliken 2010      bee-fly          <NA>
510                              Milliken 2010      bee-fly          <NA>
511                              Milliken 2010      bee-fly          <NA>
512                              Jepson Online  hummingbird          <NA>
513                              Jepson Online  hummingbird          <NA>
514                              Jepson Online  hummingbird          <NA>
515                              Jepson Online  hummingbird          <NA>
516                              Jepson Online  hummingbird          <NA>
517                              Jepson Online  hummingbird          <NA>
518                              Jepson Online  hummingbird          <NA>
519                              Jepson Online  hummingbird          <NA>
520                              Jepson Online  hummingbird          <NA>
521                              Jepson Online  hummingbird          <NA>
522                              Jepson Online         <NA>          <NA>
523                              Jepson Online         <NA>          <NA>
524                              Jepson Online         <NA>          <NA>
525                              Jepson Online         <NA>          <NA>
526                              Jepson Online         <NA>          <NA>
527                              Jepson Online         <NA>          <NA>
528                              Jepson Online         <NA>          <NA>
529                              Jepson Online         <NA>          <NA>
530                              Jepson Online         <NA>          <NA>
531                              Jepson Online         <NA>          <NA>
532                              Jepson Online         <NA>          <NA>
533                              Jepson Online         <NA>          <NA>
534                              Jepson Online         <NA>          <NA>
535                              Milliken 2010         <NA>          <NA>
536                              Milliken 2010         <NA>          <NA>
537                              Milliken 2010         <NA>          <NA>
538                              Milliken 2010         <NA>          <NA>
539                              Milliken 2010         <NA>          <NA>
540                              Milliken 2010         <NA>          <NA>
541                              Milliken 2010         <NA>          <NA>
542                              Milliken 2010         <NA>          <NA>
543                              Milliken 2010         <NA>          <NA>
544                              Milliken 2010         <NA>          <NA>
545                              Jepson Online         <NA>          <NA>
546                              Jepson Online         <NA>          <NA>
547                              Jepson Online         <NA>          <NA>
548                              Jepson Online         <NA>          <NA>
549                              Jepson Online         <NA>          <NA>
550                              Jepson Online         <NA>          <NA>
551                              Jepson Online         <NA>          <NA>
552                              Jepson Online         <NA>          <NA>
553                              Jepson Online         <NA>          <NA>
554                      Porter,  Johnson 2000    butterfly       bee-fly
555                      Porter,  Johnson 2000    butterfly       bee-fly
556                      Porter,  Johnson 2000    butterfly       bee-fly
557                      Porter,  Johnson 2000         <NA>          <NA>
558                      Porter,  Johnson 2000         <NA>          <NA>
559                      Porter,  Johnson 2000         <NA>          <NA>
560                      Porter,  Johnson 2000         <NA>          <NA>
561                      Porter,  Johnson 2000         <NA>          <NA>
562                      Porter,  Johnson 2000         <NA>          <NA>
563                      Porter,  Johnson 2000         <NA>          <NA>
564                              Jepson Online         <NA>          <NA>
565                              Jepson Online         <NA>          <NA>
566                              Jepson Online         <NA>          <NA>
567                              Jepson Online         <NA>          <NA>
568                              Jepson Online         <NA>          <NA>
569                              Jepson Online         <NA>          <NA>
570                              Jepson Online         <NA>          <NA>
571                              Jepson Online         <NA>          <NA>
572                              Jepson Online          bee       bee-fly
573                              Jepson Online          bee       bee-fly
574                              Jepson Online          bee       bee-fly
575                              Jepson Online          bee       bee-fly
576                              Jepson Online          bee       bee-fly
577                              Jepson Online          bee       bee-fly
578                              Jepson Online          bee       bee-fly
579                              Jepson Online          bee       bee-fly
580                              Jepson Online          bee       bee-fly
581                              Jepson Online          bee       bee-fly
582                              Jepson Online          bee       bee-fly
583                              Jepson Online          bee       bee-fly
584                              Jepson Online          bee       bee-fly
585                              Jepson Online          bee       bee-fly
586                              Jepson Online          bee       bee-fly
587                              Jepson Online          bee       bee-fly
588                              Jepson Online          bee       bee-fly
589                              Jepson Online          bee       bee-fly
590                              Jepson Online          bee       bee-fly
591                              Jepson Online          bee       bee-fly
592                              Jepson Online          bee       bee-fly
593                              Jepson Online          bee       bee-fly
594                              Jepson Online         <NA>          <NA>
595                              Jepson Online         <NA>          <NA>
596                              Jepson Online         <NA>          <NA>
597                              Jepson Online         <NA>          <NA>
598                              Jepson Online         <NA>          <NA>
599                              Jepson Online         <NA>          <NA>
600                              Jepson Online         <NA>          <NA>
601                              Jepson Online         <NA>          <NA>
602                              Jepson Online         <NA>          <NA>
603                              Jepson Online         <NA>          <NA>
604                              Jepson Online      bee-fly          <NA>
605                              Jepson Online      bee-fly          <NA>
606                              Jepson Online      bee-fly          <NA>
607                              Jepson Online      bee-fly          <NA>
608                              Jepson Online      bee-fly          <NA>
609                              Jepson Online      bee-fly          <NA>
610                              Jepson Online      bee-fly          <NA>
611                              Jepson Online      bee-fly          <NA>
612                              Jepson Online      bee-fly          <NA>
613                              Jepson Online      bee-fly          <NA>
614                              Jepson Online      bee-fly          <NA>
615                              Jepson Online      bee-fly          <NA>
616                              Jepson Online      bee-fly          <NA>
617                              Jepson Online      bee-fly          <NA>
618                              Jepson Online      bee-fly          <NA>
619                              Jepson Online      bee-fly          <NA>
620                              Jepson Online      bee-fly          <NA>
621                              Jepson Online      bee-fly          <NA>
622                              Jepson Online      bee-fly          <NA>
623                              Jepson Online      bee-fly          <NA>
624                              Jepson Online      bee-fly          <NA>
625                              Jepson Online      bee-fly          <NA>
626                              Jepson Online         <NA>          <NA>
627                              Jepson Online         <NA>          <NA>
628                              Jepson Online         <NA>          <NA>
629                              Jepson Online         <NA>          <NA>
630                              Jepson Online         <NA>          <NA>
631                              Jepson Online         <NA>          <NA>
632                              Jepson Online         <NA>          <NA>
633                              Jepson Online         <NA>          <NA>
634                              Jepson Online         <NA>          <NA>
635                              Jepson Online         <NA>          <NA>
636                              Jepson Online          bee       bee-fly
637                              Jepson Online          bee       bee-fly
638                              Jepson Online          bee       bee-fly
639                              Jepson Online          bee       bee-fly
640                              Jepson Online          bee       bee-fly
641                              Jepson Online          bee       bee-fly
642                              Jepson Online          bee       bee-fly
643                              Jepson Online          bee       bee-fly
644                              Jepson Online         <NA>          <NA>
645                              Jepson Online         <NA>          <NA>
646                              Jepson Online         <NA>          <NA>
647                              Jepson Online         <NA>          <NA>
648                              Jepson Online         <NA>          <NA>
649                              Jepson Online         <NA>          <NA>
650                              Jepson Online         <NA>          <NA>
651                              Jepson Online         <NA>          <NA>
652                              Jepson Online         <NA>          <NA>
653                              Jepson Online         <NA>          <NA>
654                              Jepson Online         <NA>          <NA>
655                              Jepson Online         <NA>          <NA>
656                              Jepson Online         <NA>          <NA>
657                              Jepson Online          bee          <NA>
658                              Jepson Online          bee          <NA>
659                              Jepson Online          bee          <NA>
660                              Jepson Online          bee          <NA>
661                              Jepson Online          bee          <NA>
662                              Jepson Online          bee          <NA>
663                              Jepson Online          bee          <NA>
664                              Jepson Online         <NA>          <NA>
665                              Jepson Online         <NA>          <NA>
666                              Jepson Online         <NA>          <NA>
667                              Jepson Online         <NA>          <NA>
668                              Jepson Online         <NA>          <NA>
669                              Jepson Online         <NA>          <NA>
670                              Jepson Online         <NA>          <NA>
671                              Jepson Online         <NA>          <NA>
672                              Jepson Online         <NA>          <NA>
673                              Jepson Online         <NA>          <NA>
674                              Jepson Online         <NA>          <NA>
675                              Jepson Online         <NA>          <NA>
676                              Jepson Online         <NA>          <NA>
677                              Jepson Online         <NA>          <NA>
678                              Jepson Online         <NA>          <NA>
679                              Jepson Online         <NA>          <NA>
680                              Jepson Online         <NA>          <NA>
681                              Jepson Online         <NA>          <NA>
682                              Jepson Online         <NA>          <NA>
683                              Jepson Online         <NA>          <NA>
684                              Jepson Online         <NA>          <NA>
685                              Jepson Online         <NA>          <NA>
686                              Jepson Online         <NA>          <NA>
687                              Jepson Online         <NA>          <NA>
688                              Jepson Online         <NA>          <NA>
689                              Jepson Online         <NA>          <NA>
690               Arizona-Sonora Desert Museum  hummingbird          <NA>
691               Arizona-Sonora Desert Museum  hummingbird          <NA>
692               Arizona-Sonora Desert Museum  hummingbird          <NA>
693               Arizona-Sonora Desert Museum  hummingbird          <NA>
694               Arizona-Sonora Desert Museum  hummingbird          <NA>
695               Arizona-Sonora Desert Museum  hummingbird          <NA>
696               Arizona-Sonora Desert Museum  hummingbird          <NA>
697               Arizona-Sonora Desert Museum  hummingbird          <NA>
698               Arizona-Sonora Desert Museum  hummingbird          <NA>
699               Arizona-Sonora Desert Museum  hummingbird          <NA>
700                              Jepson Online         <NA>          <NA>
701                              Jepson Online         <NA>          <NA>
702                              Jepson Online         <NA>          <NA>
703                              Jepson Online         <NA>          <NA>
704                              Jepson Online         <NA>          <NA>
705                              Jepson Online         <NA>          <NA>
706                              Jepson Online         <NA>          <NA>
707                              Jepson Online         <NA>          <NA>
708                              Jepson Online         <NA>          <NA>
709                              Jepson Online         <NA>          <NA>
710                              Jepson Online          bee          <NA>
711                              Jepson Online          bee          <NA>
712                              Jepson Online          bee          <NA>
713                              Jepson Online          bee          <NA>
714                              Jepson Online          bee          <NA>
715                              Jepson Online          bee          <NA>
716                              Jepson Online          bee          <NA>
717                              Jepson Online          bee          <NA>
718                              Jepson Online          bee          <NA>
719                              Jepson Online          bee          <NA>
720                              Jepson Online          bee          <NA>
721                              Jepson Online          bee          <NA>
722                              Jepson Online          bee          <NA>
723                              Jepson Online         <NA>          <NA>
724                              Jepson Online         <NA>          <NA>
725                              Jepson Online         <NA>          <NA>
726                              Jepson Online         <NA>          <NA>
727                              Jepson Online         <NA>          <NA>
728                              Jepson Online         <NA>          <NA>
729                              Jepson Online         <NA>          <NA>
730                              Jepson Online      bee-fly          <NA>
731                              Jepson Online      bee-fly          <NA>
732                              Jepson Online      bee-fly          <NA>
733                              Jepson Online      bee-fly          <NA>
734                              Jepson Online      bee-fly          <NA>
735                              Jepson Online      bee-fly          <NA>
736                              Jepson Online      bee-fly          <NA>
737                          Grant  Grant 1965          bee       bee-fly
738                          Grant  Grant 1965          bee       bee-fly
739                          Grant  Grant 1965          bee       bee-fly
740                          Grant  Grant 1965          bee       bee-fly
741                          Grant  Grant 1965          bee       bee-fly
742                          Grant  Grant 1965          bee       bee-fly
743                          Grant  Grant 1965          bee       bee-fly
744                          Grant  Grant 1965          bee       bee-fly
745                          Grant  Grant 1965          bee       bee-fly
746                          Grant  Grant 1965          bee       bee-fly
747                          Grant  Grant 1965          bee       bee-fly
748                          Grant  Grant 1965          bee       bee-fly
749                          Grant  Grant 1965          bee       bee-fly
750                          Grant  Grant 1965          bee       bee-fly
751                          Grant  Grant 1965          bee       bee-fly
752                          Grant  Grant 1965          bee       bee-fly
753                          Grant  Grant 1965          bee       bee-fly
754                          Grant  Grant 1965          bee       bee-fly
755                          Grant  Grant 1965          bee       bee-fly
756                          Grant  Grant 1965          bee       bee-fly
757                          Grant  Grant 1965          bee       bee-fly
758                          Grant  Grant 1965          bee       bee-fly
759                          Grant  Grant 1965          bee       bee-fly
760                          Grant  Grant 1965          bee       bee-fly
761                          Grant  Grant 1965          bee       bee-fly
762                              Jepson Online         <NA>          <NA>
763                              Jepson Online         <NA>          <NA>
764                              Jepson Online         <NA>          <NA>
765                              Jepson Online         <NA>          <NA>
766                              Jepson Online         <NA>          <NA>
767                              Jepson Online         <NA>          <NA>
768                              Jepson Online         <NA>          <NA>
769                              Jepson Online         <NA>          <NA>
770                              Jepson Online         <NA>          <NA>
771                              Jepson Online         <NA>          <NA>
772                              Jepson Online         <NA>          <NA>
773                              Jepson Online         <NA>          <NA>
774                              Jepson Online         <NA>          <NA>
775                              Jepson Online         <NA>          <NA>
776                            Flora Mendocina         <NA>          <NA>
777                            Flora Mendocina         <NA>          <NA>
778                            Flora Mendocina         <NA>          <NA>
779                            Flora Mendocina         <NA>          <NA>
780                            Flora Mendocina         <NA>          <NA>
781                            Flora Mendocina         <NA>          <NA>
782                            Flora Mendocina         <NA>          <NA>
783                            Flora Mendocina         <NA>          <NA>
784                            Flora Mendocina         <NA>          <NA>
785                            Flora Mendocina         <NA>          <NA>
786                              Jepson Online          bee       bee-fly
787                              Jepson Online          bee       bee-fly
788                              Jepson Online          bee       bee-fly
789                              Jepson Online          bee       bee-fly
790                              Jepson Online          bee       bee-fly
791                              Jepson Online          bee       bee-fly
792                              Jepson Online          bee       bee-fly
793                              Jepson Online         <NA>          <NA>
794                              Jepson Online         <NA>          <NA>
795                              Jepson Online         <NA>          <NA>
796                              Jepson Online         <NA>          <NA>
797                              Jepson Online         <NA>          <NA>
798                              Jepson Online         <NA>          <NA>
799                              Jepson Online         <NA>          <NA>
800                              Jepson Online         <NA>          <NA>
801                              Jepson Online         <NA>          <NA>
802                              Jepson Online         <NA>          <NA>
803                              Jepson Online         <NA>          <NA>
804                              Jepson Online         <NA>          <NA>
805                              Jepson Online         <NA>          <NA>
806                              Jepson Online         <NA>          <NA>
807                              Jepson Online         <NA>          <NA>
808                              Jepson Online         <NA>          <NA>
809                              Jepson Online         <NA>          <NA>
810                              Jepson Online         <NA>          <NA>
811                              Jepson Online         <NA>          <NA>
812                              Jepson Online         <NA>          <NA>
813                              Jepson Online         <NA>          <NA>
814                              Jepson Online         <NA>          <NA>
815                              Jepson Online         <NA>          <NA>
816                              Jepson Online         <NA>          <NA>
817                              Jepson Online         <NA>          <NA>
818                              Jepson Online         <NA>          <NA>
819                              Jepson Online         <NA>          <NA>
820                              Jepson Online         <NA>          <NA>
821                              Jepson Online         <NA>          <NA>
822                              Jepson Online         <NA>          <NA>
823                              Jepson Online         <NA>          <NA>
824                              Jepson Online         <NA>          <NA>
825                              Jepson Online         <NA>          <NA>
826                              Jepson Online         <NA>          <NA>
827                              Jepson Online         <NA>          <NA>
828                              Jepson Online         <NA>          <NA>
829                              Jepson Online          bee       bee-fly
830                              Jepson Online          bee       bee-fly
831                              Jepson Online          bee       bee-fly
832                              Jepson Online          bee       bee-fly
833                              Jepson Online          bee       bee-fly
834                              Jepson Online          bee       bee-fly
835                              Jepson Online          bee       bee-fly
836                              Jepson Online          bee       bee-fly
837                              Jepson Online          bee       bee-fly
838                              Jepson Online          bee       bee-fly
839                              Jepson Online          bee          <NA>
840                              Jepson Online          bee          <NA>
841                              Jepson Online          bee          <NA>
842                              Jepson Online          bee          <NA>
843                              Jepson Online         <NA>          <NA>
844                              Jepson Online         <NA>          <NA>
845                              Jepson Online         <NA>          <NA>
846                              Jepson Online         <NA>          <NA>
847                  Herbarium sheet MO2926797         <NA>          <NA>
848                  Herbarium sheet MO2926797         <NA>          <NA>
849                  Herbarium sheet MO2926797         <NA>          <NA>
850                  Herbarium sheet MO2926797         <NA>          <NA>
851                              Jepson Online         <NA>          <NA>
852                              Jepson Online         <NA>          <NA>
853                              Jepson Online         <NA>          <NA>
854                              Jepson Online         <NA>          <NA>
855                              Jepson Online         <NA>          <NA>
856                              Jepson Online         <NA>          <NA>
857                              Jepson Online         <NA>          <NA>
858                              Jepson Online         <NA>          <NA>
859                              Jepson Online         <NA>          <NA>
860                              Jepson Online         <NA>          <NA>
861                              Jepson Online         <NA>          <NA>
862                              Jepson Online         <NA>          <NA>
863                              Jepson Online         <NA>          <NA>
864                              Jepson Online         <NA>          <NA>
865                              Jepson Online         <NA>          <NA>
866                              Jepson Online         <NA>          <NA>
867                              Jepson Online         <NA>          <NA>
868                              Jepson Online         <NA>          <NA>
869                              Jepson Online         <NA>          <NA>
870                              Jepson Online         <NA>          <NA>
871                              Jepson Online         <NA>          <NA>
872                              Jepson Online         <NA>          <NA>
873                              Jepson Online         <NA>          <NA>
874                              Jepson Online         <NA>          <NA>
875                              Jepson Online         <NA>          <NA>
876                              Jepson Online         <NA>          <NA>
877                              Jepson Online         <NA>          <NA>
878                              Jepson Online         <NA>          <NA>
879                              Jepson Online         <NA>          <NA>
880                              Jepson Online         <NA>          <NA>
881                              Jepson Online         <NA>          <NA>
882                              Jepson Online         <NA>          <NA>
883                              Jepson Online         <NA>          <NA>
884                              Jepson Online         <NA>          <NA>
885                              Jepson Online         <NA>          <NA>
886                              Jepson Online         <NA>          <NA>
887                              Jepson Online         <NA>          <NA>
888                              Jepson Online         <NA>          <NA>
889                              Jepson Online         <NA>          <NA>
890                              Jepson Online         <NA>          <NA>
891                              Jepson Online         <NA>          <NA>
892                              Jepson Online         <NA>          <NA>
893                              Jepson Online         <NA>          <NA>
894                              Jepson Online         <NA>          <NA>
895                              Jepson Online         <NA>          <NA>
896                              Jepson Online         <NA>          <NA>
897                              Jepson Online         <NA>          <NA>
898                              Jepson Online         <NA>          <NA>
899                              Jepson Online         <NA>          <NA>
900                              Jepson Online         <NA>          <NA>
901                              Jepson Online         <NA>          <NA>
902                              Jepson Online         <NA>          <NA>
903                              Jepson Online         <NA>          <NA>
904                              Jepson Online         <NA>          <NA>
905                              Jepson Online         <NA>          <NA>
906                              Jepson Online         <NA>          <NA>
907                              Jepson Online         <NA>          <NA>
908                              Jepson Online         <NA>          <NA>
909                              Jepson Online         <NA>          <NA>
910                          Grant  Grant 1965      bee-fly          <NA>
911                          Grant  Grant 1965      bee-fly          <NA>
912                          Grant  Grant 1965      bee-fly          <NA>
913                          Grant  Grant 1965      bee-fly          <NA>
914                          Grant  Grant 1965      bee-fly          <NA>
915                          Grant  Grant 1965      bee-fly          <NA>
916                          Grant  Grant 1965      bee-fly          <NA>
917                          Grant  Grant 1965      bee-fly          <NA>
918                          Grant  Grant 1965      bee-fly          <NA>
919                          Grant  Grant 1965      bee-fly          <NA>
920                          Grant  Grant 1965      bee-fly          <NA>
921                          Grant  Grant 1965      bee-fly          <NA>
922                          Grant  Grant 1965      bee-fly          <NA>
923                          Grant  Grant 1965      bee-fly          <NA>
924                          Grant  Grant 1965      bee-fly          <NA>
925                          Grant  Grant 1965      bee-fly          <NA>
926                          Grant  Grant 1965      bee-fly          <NA>
927                          Grant  Grant 1965      bee-fly          <NA>
928                          Grant  Grant 1965      bee-fly          <NA>
929                              Jepson Online         <NA>          <NA>
930                              Jepson Online         <NA>          <NA>
931                              Jepson Online         <NA>          <NA>
932                              Jepson Online         <NA>          <NA>
933                          Grant  Grant 1965          bee       bee-fly
934                          Grant  Grant 1965          bee       bee-fly
935                          Grant  Grant 1965          bee       bee-fly
936                          Grant  Grant 1965          bee       bee-fly
937                          Grant  Grant 1965          bee       bee-fly
938                          Grant  Grant 1965          bee       bee-fly
939                          Grant  Grant 1965          bee       bee-fly
940                          Grant  Grant 1965          bee       bee-fly
941                          Grant  Grant 1965          bee       bee-fly
942                          Grant  Grant 1965          bee       bee-fly
943                          Grant  Grant 1965          bee       bee-fly
944                          Grant  Grant 1965          bee       bee-fly
945                          Grant  Grant 1965          bee       bee-fly
946                          Grant  Grant 1965          bee       bee-fly
947                          Grant  Grant 1965          bee       bee-fly
948                          Grant  Grant 1965          bee       bee-fly
949                        Montana Field Guide         <NA>          <NA>
950                        Montana Field Guide         <NA>          <NA>
951                        Montana Field Guide         <NA>          <NA>
952                        Montana Field Guide         <NA>          <NA>
953                        Montana Field Guide         <NA>          <NA>
954                        Montana Field Guide         <NA>          <NA>
955                        Montana Field Guide         <NA>          <NA>
956                        Montana Field Guide         <NA>          <NA>
957                        Montana Field Guide         <NA>          <NA>
958                        Montana Field Guide         <NA>          <NA>
959                  Chileflora Plant Database         <NA>          <NA>
960                  Chileflora Plant Database         <NA>          <NA>
961                  Chileflora Plant Database         <NA>          <NA>
962                  Chileflora Plant Database         <NA>          <NA>
963                  Chileflora Plant Database         <NA>          <NA>
964                  Chileflora Plant Database         <NA>          <NA>
965                          Grant  Grant 1965          bee          <NA>
966                          Grant  Grant 1965          bee          <NA>
967                          Grant  Grant 1965          bee          <NA>
968                          Grant  Grant 1965          bee          <NA>
969                          Grant  Grant 1965          bee          <NA>
970                          Grant  Grant 1965          bee          <NA>
971                          Grant  Grant 1965          bee          <NA>
972                          Grant  Grant 1965          bee          <NA>
973                          Grant  Grant 1965          bee          <NA>
974                          Grant  Grant 1965         <NA>          <NA>
975                          Grant  Grant 1965         <NA>          <NA>
976                          Grant  Grant 1965         <NA>          <NA>
977                          Grant  Grant 1965         <NA>          <NA>
978                          Grant  Grant 1965         <NA>          <NA>
979                          Grant  Grant 1965         <NA>          <NA>
980                          Grant  Grant 1965         <NA>          <NA>
981                          Grant  Grant 1965         <NA>          <NA>
982                          Grant  Grant 1965         <NA>          <NA>
983                          Grant  Grant 1965         <NA>          <NA>
984                          Grant  Grant 1965         <NA>          <NA>
985                          Grant  Grant 1965         <NA>          <NA>
986                          Grant  Grant 1965         <NA>          <NA>
987                          Grant  Grant 1965         <NA>          <NA>
988                          Grant  Grant 1965         <NA>          <NA>
989                          Grant  Grant 1965         <NA>          <NA>
990        Lady Bird Johnson Wildflower Center         <NA>          <NA>
991        Lady Bird Johnson Wildflower Center         <NA>          <NA>
992        Lady Bird Johnson Wildflower Center         <NA>          <NA>
993        Lady Bird Johnson Wildflower Center         <NA>          <NA>
994        Lady Bird Johnson Wildflower Center         <NA>          <NA>
995        Lady Bird Johnson Wildflower Center         <NA>          <NA>
996        Lady Bird Johnson Wildflower Center         <NA>          <NA>
997                          Grant  Grant 1965         <NA>          <NA>
998                          Grant  Grant 1965         <NA>          <NA>
999                          Grant  Grant 1965         <NA>          <NA>
1000                         Grant  Grant 1965         <NA>          <NA>
1001                         Grant  Grant 1965         <NA>          <NA>
1002                         Grant  Grant 1965         <NA>          <NA>
1003                         Grant  Grant 1965         <NA>          <NA>
1004                         Grant  Grant 1965         <NA>          <NA>
1005                         Grant  Grant 1965         <NA>          <NA>
1006                         Grant  Grant 1965         <NA>          <NA>
1007                         Grant  Grant 1965         <NA>          <NA>
1008                             Jepson Online         <NA>          <NA>
1009                             Jepson Online         <NA>          <NA>
1010                             Jepson Online         <NA>          <NA>
1011                             Jepson Online         <NA>          <NA>
1012                             Jepson Online         <NA>          <NA>
1013                             Jepson Online         <NA>          <NA>
1014                             Jepson Online         <NA>          <NA>
1015                       Grant,  Wilken 1988         <NA>          <NA>
1016                       Grant,  Wilken 1988         <NA>          <NA>
1017                       Grant,  Wilken 1988         <NA>          <NA>
1018                       Grant,  Wilken 1988         <NA>          <NA>
1019                       Grant,  Wilken 1988         <NA>          <NA>
1020                       Grant,  Wilken 1988         <NA>          <NA>
1021                       Grant,  Wilken 1988         <NA>          <NA>
1022                       Grant,  Wilken 1988         <NA>          <NA>
1023                       Grant,  Wilken 1988         <NA>          <NA>
1024                       Grant,  Wilken 1988         <NA>          <NA>
1025                         Grant  Grant 1965  hummingbird          <NA>
1026                         Grant  Grant 1965  hummingbird          <NA>
1027                         Grant  Grant 1965  hummingbird          <NA>
1028                         Grant  Grant 1965  hummingbird          <NA>
1029                         Grant  Grant 1965  hummingbird          <NA>
1030                         Grant  Grant 1965  hummingbird          <NA>
1031                         Grant  Grant 1965  hummingbird          <NA>
1032                         Grant  Grant 1965  hummingbird          <NA>
1033                         Grant  Grant 1965  hummingbird          <NA>
1034                         Grant  Grant 1965  hummingbird          <NA>
1035                         Grant  Grant 1965  hummingbird          <NA>
1036                         Grant  Grant 1965  hummingbird          <NA>
1037                         Grant  Grant 1965  hummingbird          <NA>
1038                         Grant  Grant 1965  hummingbird          <NA>
1039                         Grant  Grant 1965  hummingbird          <NA>
1040                         Grant  Grant 1965  hummingbird          <NA>
1041                         Grant  Grant 1965  hummingbird          <NA>
1042                         Grant  Grant 1965  hummingbird          <NA>
1043                         Grant  Grant 1965  hummingbird          <NA>
1044                         Grant  Grant 1965  hummingbird          <NA>
1045                         Grant  Grant 1965  hummingbird          <NA>
1046                         Grant  Grant 1965  hummingbird          <NA>
1047                         Grant  Grant 1965      beetles           bee
1048                         Grant  Grant 1965      beetles           bee
1049                         Grant  Grant 1965      beetles           bee
1050                         Grant  Grant 1965      beetles           bee
1051                         Grant  Grant 1965      beetles           bee
1052                         Grant  Grant 1965      beetles           bee
1053                         Grant  Grant 1965      beetles           bee
1054                         Grant  Grant 1965      beetles           bee
1055                         Grant  Grant 1965      beetles           bee
1056                         Grant  Grant 1965      beetles           bee
1057                             Jepson Online         <NA>          <NA>
1058                             Jepson Online         <NA>          <NA>
1059                             Jepson Online         <NA>          <NA>
1060                             Jepson Online         <NA>          <NA>
1061                             Jepson Online         <NA>          <NA>
1062                             Jepson Online         <NA>          <NA>
1063                             Jepson Online         <NA>          <NA>
1064           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1065           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1066           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1067           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1068           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1069           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1070           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1071           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1072           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1073           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1074           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1075           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1076           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1077           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1078           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1079           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1080           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1081           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1082           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1083           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1084           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1085           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1086           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1087           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1088           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1089           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1090           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1091           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1092           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1093           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1094           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1095           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1096           Thomas Juenger Lab, Univ. Texas      bee-fly          <NA>
1097           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1098           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1099           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1100           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1101           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1102           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1103           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1104           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1105           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1106           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1107           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1108           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1109           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1110           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1111           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1112           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1113           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1114           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1115           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1116           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1117                         Grant  Grant 1965     hawkmoth          <NA>
1118                         Grant  Grant 1965     hawkmoth          <NA>
1119                         Grant  Grant 1965     hawkmoth          <NA>
1120                         Grant  Grant 1965     hawkmoth          <NA>
1121                         Grant  Grant 1965     hawkmoth          <NA>
1122                         Grant  Grant 1965     hawkmoth          <NA>
1123                         Grant  Grant 1965     hawkmoth          <NA>
1124                         Grant  Grant 1965     hawkmoth          <NA>
1125                         Grant  Grant 1965     hawkmoth          <NA>
1126                         Grant  Grant 1965     hawkmoth          <NA>
1127                         Grant  Grant 1965     hawkmoth          <NA>
1128                         Grant  Grant 1965     hawkmoth          <NA>
1129                         Grant  Grant 1965     hawkmoth          <NA>
1130                         Grant  Grant 1965     hawkmoth          <NA>
1131                         Grant  Grant 1965     hawkmoth          <NA>
1132                         Grant  Grant 1965     hawkmoth          <NA>
1133                         Grant  Grant 1965     hawkmoth          <NA>
1134                         Grant  Grant 1965     hawkmoth          <NA>
1135                         Grant  Grant 1965     hawkmoth          <NA>
1136                         Grant  Grant 1965     hawkmoth          <NA>
1137                         Grant  Grant 1965     hawkmoth          <NA>
1138                         Grant  Grant 1965     hawkmoth          <NA>
1139                         Grant  Grant 1965    butterfly           bee
1140                         Grant  Grant 1965    butterfly           bee
1141                         Grant  Grant 1965    butterfly           bee
1142                         Grant  Grant 1965    butterfly           bee
1143                         Grant  Grant 1965    butterfly           bee
1144                         Grant  Grant 1965    butterfly           bee
1145                         Grant  Grant 1965    butterfly           bee
1146                         Grant  Grant 1965    butterfly           bee
1147                         Grant  Grant 1965    butterfly           bee
1148                         Grant  Grant 1965    butterfly           bee
1149                         Grant  Grant 1965    butterfly           bee
1150                         Grant  Grant 1965    butterfly           bee
1151                         Grant  Grant 1965    butterfly           bee
1152                         Grant  Grant 1965    butterfly           bee
1153                         Grant  Grant 1965    butterfly           bee
1154                         Grant  Grant 1965    butterfly           bee
1155                         Grant  Grant 1965    butterfly           bee
1156                         Grant  Grant 1965    butterfly           bee
1157                         Grant  Grant 1965    butterfly           bee
1158                       Grant,  Wilken 1988         <NA>          <NA>
1159                       Grant,  Wilken 1988         <NA>          <NA>
1160                       Grant,  Wilken 1988         <NA>          <NA>
1161                       Grant,  Wilken 1988         <NA>          <NA>
1162                       Grant,  Wilken 1988         <NA>          <NA>
1163                       Grant,  Wilken 1988         <NA>          <NA>
1164                       Grant,  Wilken 1988         <NA>          <NA>
1165                         Grant  Grant 1965          bee          <NA>
1166                         Grant  Grant 1965          bee          <NA>
1167                         Grant  Grant 1965          bee          <NA>
1168                         Grant  Grant 1965          bee          <NA>
1169                         Grant  Grant 1965          bee          <NA>
1170                         Grant  Grant 1965          bee          <NA>
1171                         Grant  Grant 1965          bee          <NA>
1172                         Grant  Grant 1965          bee          <NA>
1173                         Grant  Grant 1965          bee          <NA>
1174                         Grant  Grant 1965          bee          <NA>
1175                         Grant  Grant 1965          bee          <NA>
1176                         Grant  Grant 1965          bee          <NA>
1177                         Grant  Grant 1965          bee          <NA>
1178                         Grant  Grant 1965          bee          <NA>
1179                         Grant  Grant 1965          bee          <NA>
1180                         Grant  Grant 1965          bee          <NA>
1181                         Grant  Grant 1965          bee          <NA>
1182                         Grant  Grant 1965          bee          <NA>
1183                         Grant  Grant 1965          bee          <NA>
1184                         Grant  Grant 1965          bee          <NA>
1185                         Grant  Grant 1965          bee          <NA>
1186                         Grant  Grant 1965          bee          <NA>
1187                         Grant  Grant 1965          bee          <NA>
1188                         Grant  Grant 1965          bee          <NA>
1189                         Grant  Grant 1965          bee          <NA>
1190                         Grant  Grant 1965          bee          <NA>
1191                         Grant  Grant 1965          bee          <NA>
1192                         Grant  Grant 1965          bee          <NA>
1193                         Grant  Grant 1965          bee          <NA>
1194                         Grant  Grant 1965          bee          <NA>
1195                         Grant  Grant 1965          bee          <NA>
1196                         Grant  Grant 1965          bee          <NA>
1197                         Grant  Grant 1965          bee          <NA>
1198                         Grant  Grant 1965          bee          <NA>
1199                         Grant  Grant 1965          bee          <NA>
1200                         Grant  Grant 1965          bee          <NA>
1201                         Grant  Grant 1965          bee          <NA>
1202                         Grant  Grant 1965          bee          <NA>
1203                         Grant  Grant 1965          bee          <NA>
1204                         Grant  Grant 1965          bee          <NA>
1205                         Grant  Grant 1965          bee          <NA>
1206                             Jepson Online         <NA>          <NA>
1207                             Jepson Online         <NA>          <NA>
1208                             Jepson Online         <NA>          <NA>
1209                             Jepson Online         <NA>          <NA>
1210                             Jepson Online         <NA>          <NA>
1211                             Jepson Online         <NA>          <NA>
1212                             Jepson Online         <NA>          <NA>
1213                             Jepson Online         <NA>          <NA>
1214                             Jepson Online         <NA>          <NA>
1215                             Jepson Online         <NA>          <NA>
1216                             Jepson Online         <NA>          <NA>
1217                             Jepson Online         <NA>          <NA>
1218           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1219           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1220           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1221           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1222           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1223           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1224           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1225           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1226           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1227           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1228           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1229           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1230           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1231           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1232           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1233           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1234           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1235           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1236           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1237           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1238           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1239           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1240           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1241           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1242           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1243           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1244           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1245           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1246           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1247           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1248           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1249           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1250           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1251           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1252           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1253           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1254           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1255           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1256           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1257           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1258           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1259           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1260           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1261           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1262           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1263           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1264           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1265           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1266           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1267           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1268           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1269                         Grant  Grant 1965  hummingbird          <NA>
1270                         Grant  Grant 1965  hummingbird          <NA>
1271                         Grant  Grant 1965  hummingbird          <NA>
1272                         Grant  Grant 1965  hummingbird          <NA>
1273                         Grant  Grant 1965  hummingbird          <NA>
1274                         Grant  Grant 1965  hummingbird          <NA>
1275                         Grant  Grant 1965  hummingbird          <NA>
1276           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1277           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1278           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1279           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1280           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1281           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1282                         Grant  Grant 1965  hummingbird          <NA>
1283                         Grant  Grant 1965  hummingbird          <NA>
1284                         Grant  Grant 1965     hawkmoth          <NA>
1285                         Grant  Grant 1965     hawkmoth          <NA>
1286                         Grant  Grant 1965     hawkmoth          <NA>
1287                         Grant  Grant 1965     hawkmoth          <NA>
1288                         Grant  Grant 1965     hawkmoth          <NA>
1289                         Grant  Grant 1965     hawkmoth          <NA>
1290                         Grant  Grant 1965     hawkmoth          <NA>
1291                         Grant  Grant 1965     hawkmoth          <NA>
1292                         Grant  Grant 1965     hawkmoth          <NA>
1293                         Grant  Grant 1965     hawkmoth          <NA>
1294                         Grant  Grant 1965     hawkmoth          <NA>
1295                         Grant  Grant 1965     hawkmoth          <NA>
1296                         Grant  Grant 1965     hawkmoth          <NA>
1297                         Grant  Grant 1965     hawkmoth          <NA>
1298                         Grant  Grant 1965     hawkmoth          <NA>
1299                         Grant  Grant 1965     hawkmoth          <NA>
1300                         Grant  Grant 1965     hawkmoth          <NA>
1301                         Grant  Grant 1965     hawkmoth          <NA>
1302                         Grant  Grant 1965     hawkmoth          <NA>
1303                         Grant  Grant 1965     hawkmoth          <NA>
1304                         Grant  Grant 1965     hawkmoth          <NA>
1305                         Grant  Grant 1965     hawkmoth          <NA>
1306                         Grant  Grant 1965     hawkmoth          <NA>
1307           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1308           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1309           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1310           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1311           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1312           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1313           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1314           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1315           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1316           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1317           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1318           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1319           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1320           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1321           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1322           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1323           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1324           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1325           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1326           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1327                                 CalPhotos      bee-fly          <NA>
1328                                 CalPhotos      bee-fly          <NA>
1329                                 CalPhotos      bee-fly          <NA>
1330                                 CalPhotos      bee-fly          <NA>
1331                                 CalPhotos      bee-fly          <NA>
1332                                 CalPhotos      bee-fly          <NA>
1333                                 CalPhotos      bee-fly          <NA>
1334                                 CalPhotos      bee-fly          <NA>
1335                                 CalPhotos      bee-fly          <NA>
1336                                 CalPhotos      bee-fly          <NA>
1337                                 CalPhotos      bee-fly          <NA>
1338                                 CalPhotos      bee-fly          <NA>
1339                                 CalPhotos      bee-fly          <NA>
1340                                 CalPhotos      bee-fly          <NA>
1341                                 CalPhotos      bee-fly          <NA>
1342                                 CalPhotos      bee-fly          <NA>
1343                                 CalPhotos      bee-fly          <NA>
1344                                 CalPhotos      bee-fly          <NA>
1345                                 CalPhotos      bee-fly          <NA>
1346                                 CalPhotos      bee-fly          <NA>
1347                                 CalPhotos      bee-fly          <NA>
1348                                 CalPhotos      bee-fly          <NA>
1349                                 CalPhotos      bee-fly          <NA>
1350                                 CalPhotos      bee-fly          <NA>
1351                             Jepson Online         <NA>          <NA>
1352                             Jepson Online         <NA>          <NA>
1353                             Jepson Online         <NA>          <NA>
1354                             Jepson Online         <NA>          <NA>
1355                             Jepson Online         <NA>          <NA>
1356                             Jepson Online         <NA>          <NA>
1357                             Jepson Online         <NA>          <NA>
1358                             Jepson Online         <NA>          <NA>
1359                             Jepson Online         <NA>          <NA>
1360                             Jepson Online         <NA>          <NA>
1361                             Jepson Online         <NA>          <NA>
1362                             Jepson Online         <NA>          <NA>
1363                             Jepson Online         <NA>          <NA>
1364                             Jepson Online         <NA>          <NA>
1365                             Jepson Online         <NA>          <NA>
1366                             Jepson Online         <NA>          <NA>
1367                             Jepson Online         <NA>          <NA>
1368                             Jepson Online         <NA>          <NA>
1369                             Jepson Online         <NA>          <NA>
1370                             Jepson Online         <NA>          <NA>
1371                             Jepson Online         <NA>          <NA>
1372                             Jepson Online         <NA>          <NA>
1373                             Jepson Online         <NA>          <NA>
1374                             Jepson Online         <NA>          <NA>
1375                             Jepson Online         <NA>          <NA>
1376                             Jepson Online         <NA>          <NA>
1377                             Jepson Online         <NA>          <NA>
1378                             Jepson Online        flies          <NA>
1379                             Jepson Online        flies          <NA>
1380                             Jepson Online        flies          <NA>
1381                             Jepson Online        flies          <NA>
1382                             Jepson Online        flies          <NA>
1383                             Jepson Online        flies          <NA>
1384                             Jepson Online        flies          <NA>
1385                             Jepson Online        flies          <NA>
1386                             Jepson Online        flies          <NA>
1387                             Jepson Online        flies          <NA>
1388                             Jepson Online         <NA>          <NA>
1389                             Jepson Online         <NA>          <NA>
1390                             Jepson Online         <NA>          <NA>
1391                             Jepson Online         <NA>          <NA>
1392                             Jepson Online         <NA>          <NA>
1393                             Jepson Online         <NA>          <NA>
1394                             Jepson Online         <NA>          <NA>
1395                             Jepson Online      bee-fly          <NA>
1396                             Jepson Online      bee-fly          <NA>
1397                             Jepson Online      bee-fly          <NA>
1398                             Jepson Online      bee-fly          <NA>
1399                             Jepson Online      bee-fly          <NA>
1400                             Jepson Online      bee-fly          <NA>
1401                             Jepson Online      bee-fly          <NA>
1402                             Jepson Online      bee-fly          <NA>
1403                             Jepson Online      bee-fly          <NA>
1404                             Jepson Online      bee-fly          <NA>
1405                             Jepson Online      bee-fly          <NA>
1406                             Jepson Online      bee-fly          <NA>
1407                             Jepson Online      bee-fly          <NA>
1408                             Jepson Online         <NA>          <NA>
1409                             Jepson Online         <NA>          <NA>
1410                             Jepson Online         <NA>          <NA>
1411                             Jepson Online         <NA>          <NA>
1412                             Jepson Online      bee-fly          <NA>
1413                             Jepson Online      bee-fly          <NA>
1414                             Jepson Online      bee-fly          <NA>
1415                             Jepson Online      bee-fly          <NA>
1416                             Jepson Online         <NA>          <NA>
1417                             Jepson Online         <NA>          <NA>
1418                             Jepson Online         <NA>          <NA>
1419                             Jepson Online         <NA>          <NA>
1420                             Jepson Online         <NA>          <NA>
1421                             Jepson Online         <NA>          <NA>
1422                             Jepson Online         <NA>          <NA>
1423                             Jepson Online         <NA>          <NA>
1424                             Jepson Online         <NA>          <NA>
1425                             Jepson Online         <NA>          <NA>
1426                             Jepson Online         <NA>          <NA>
1427                             Jepson Online         <NA>          <NA>
1428                             Jepson Online         <NA>          <NA>
1429                             Jepson Online         <NA>          <NA>
1430                             Jepson Online         <NA>          <NA>
1431                             Jepson Online         <NA>          <NA>
1432                             Jepson Online         <NA>          <NA>
1433                             Jepson Online         <NA>          <NA>
1434                             Jepson Online         <NA>          <NA>
1435                             Jepson Online         <NA>          <NA>
1436                             Jepson Online         <NA>          <NA>
1437                             Jepson Online         <NA>          <NA>
1438                             Jepson Online         <NA>          <NA>
1439                             Jepson Online         <NA>          <NA>
1440                             Jepson Online         <NA>          <NA>
1441                             Jepson Online         <NA>          <NA>
1442                             Jepson Online         <NA>          <NA>
1443                             Jepson Online          bee          <NA>
1444                             Jepson Online          bee          <NA>
1445                             Jepson Online          bee          <NA>
1446                             Jepson Online          bee          <NA>
1447                             Jepson Online          bee          <NA>
1448                             Jepson Online          bee          <NA>
1449                             Jepson Online          bee          <NA>
1450                             Jepson Online         <NA>          <NA>
1451                             Jepson Online         <NA>          <NA>
1452                             Jepson Online         <NA>          <NA>
1453                             Jepson Online         <NA>          <NA>
1454                             Jepson Online         <NA>          <NA>
1455                             Jepson Online         <NA>          <NA>
1456                             Jepson Online         <NA>          <NA>
1457                             Jepson Online         <NA>          <NA>
1458                             Jepson Online         <NA>          <NA>
1459                             Jepson Online         <NA>          <NA>
1460                             Jepson Online         <NA>          <NA>
1461                             Jepson Online         <NA>          <NA>
1462                             Jepson Online         <NA>          <NA>
1463                             Jepson Online         <NA>          <NA>
1464                             Jepson Online         <NA>          <NA>
1465                             Jepson Online         <NA>          <NA>
1466                             Jepson Online         <NA>          <NA>
1467                             Jepson Online         <NA>          <NA>
1468                             Jepson Online         <NA>          <NA>
1469                             Jepson Online         <NA>          <NA>
1470                             Jepson Online         <NA>          <NA>
1471                             Jepson Online         <NA>          <NA>
1472                             Jepson Online         <NA>          <NA>
1473                             Jepson Online         <NA>          <NA>
1474                             Jepson Online          bee       bee-fly
1475                             Jepson Online          bee       bee-fly
1476                             Jepson Online          bee       bee-fly
1477                             Jepson Online          bee       bee-fly
1478                             Milliken 2010         <NA>          <NA>
1479                             Milliken 2010         <NA>          <NA>
1480                             Milliken 2010         <NA>          <NA>
1481                             Milliken 2010         <NA>          <NA>
1482                             Milliken 2010         <NA>          <NA>
1483                             Milliken 2010         <NA>          <NA>
1484                             Milliken 2010         <NA>          <NA>
1485                             Jepson Online         <NA>          <NA>
1486                             Jepson Online         <NA>          <NA>
1487                             Jepson Online         <NA>          <NA>
1488                             Jepson Online         <NA>          <NA>
1489                             Jepson Online         <NA>          <NA>
1490                             Jepson Online         <NA>          <NA>
1491                             Jepson Online         <NA>          <NA>
1492                             Jepson Online         <NA>          <NA>
1493                             Jepson Online         <NA>          <NA>
1494                             Jepson Online         <NA>          <NA>
1495                             Jepson Online         <NA>          <NA>
1496                             Jepson Online    butterfly       bee-fly
1497                             Jepson Online    butterfly       bee-fly
1498                             Jepson Online    butterfly       bee-fly
1499                             Jepson Online    butterfly       bee-fly
1500                             Jepson Online    butterfly       bee-fly
1501                             Jepson Online    butterfly       bee-fly
1502                             Jepson Online    butterfly       bee-fly
1503                             Jepson Online    butterfly       bee-fly
1504                             Jepson Online    butterfly       bee-fly
1505                             Jepson Online    butterfly       bee-fly
1506                             Jepson Online    butterfly       bee-fly
1507                             Jepson Online    butterfly       bee-fly
1508                             Jepson Online    butterfly       bee-fly
1509                             Jepson Online         <NA>          <NA>
1510                             Jepson Online         <NA>          <NA>
1511                             Jepson Online         <NA>          <NA>
1512                             Jepson Online         <NA>          <NA>
1513                             Jepson Online         <NA>          <NA>
1514                             Jepson Online         <NA>          <NA>
1515                             Jepson Online         <NA>          <NA>
1516                             Jepson Online         <NA>          <NA>
1517                             Jepson Online         <NA>          <NA>
1518                             Jepson Online         <NA>          <NA>
1519                             Jepson Online         <NA>          <NA>
1520                             Jepson Online      bee-fly          <NA>
1521                             Jepson Online      bee-fly          <NA>
1522                             Jepson Online      bee-fly          <NA>
1523                             Jepson Online      bee-fly          <NA>
1524                             Jepson Online      bee-fly          <NA>
1525                             Jepson Online      bee-fly          <NA>
1526                             Jepson Online      bee-fly          <NA>
1527                             Jepson Online         <NA>          <NA>
1528                             Jepson Online         <NA>          <NA>
1529                             Jepson Online         <NA>          <NA>
1530                             Jepson Online         <NA>          <NA>
1531                             Jepson Online         <NA>          <NA>
1532                             Jepson Online         <NA>          <NA>
1533                             Jepson Online         <NA>          <NA>
1534                             Jepson Online         <NA>          <NA>
1535                             Jepson Online         <NA>          <NA>
1536                             Jepson Online         <NA>          <NA>
1537                             Jepson Online         <NA>          <NA>
1538                             Jepson Online         <NA>          <NA>
1539                             Jepson Online         <NA>          <NA>
1540                             Jepson Online         <NA>          <NA>
1541                             Jepson Online         <NA>          <NA>
1542                             Jepson Online         <NA>          <NA>
1543                             Jepson Online         <NA>          <NA>
1544                             Jepson Online         <NA>          <NA>
1545                             Jepson Online         <NA>          <NA>
1546                             Jepson Online         <NA>          <NA>
1547                             Jepson Online         <NA>          <NA>
1548                             Jepson Online         <NA>          <NA>
1549                             Jepson Online         <NA>          <NA>
1550                             Jepson Online         <NA>          <NA>
1551                             Jepson Online         <NA>          <NA>
1552                             Jepson Online         <NA>          <NA>
1553                                 CalPhotos         <NA>          <NA>
1554                                 CalPhotos         <NA>          <NA>
1555                                 CalPhotos         <NA>          <NA>
1556                                 CalPhotos         <NA>          <NA>
1557                                 CalPhotos         <NA>          <NA>
1558                                 CalPhotos         <NA>          <NA>
1559                                 CalPhotos         <NA>          <NA>
1560                                 CalPhotos         <NA>          <NA>
1561                                 CalPhotos         <NA>          <NA>
1562                                 CalPhotos         <NA>          <NA>
1563                             Jepson Online    butterfly    hawkmoths,
1564                             Jepson Online    butterfly    hawkmoths,
1565                             Jepson Online    butterfly    hawkmoths,
1566                             Jepson Online    butterfly    hawkmoths,
1567                             Jepson Online    butterfly    hawkmoths,
1568                             Jepson Online    butterfly    hawkmoths,
1569                             Jepson Online    butterfly    hawkmoths,
1570                             Jepson Online    butterfly    hawkmoths,
1571                             Jepson Online    butterfly    hawkmoths,
1572                             Jepson Online    butterfly    hawkmoths,
1573                             Jepson Online    butterfly    hawkmoths,
1574                             Jepson Online    butterfly    hawkmoths,
1575                             Jepson Online    butterfly    hawkmoths,
1576                             Jepson Online    butterfly    hawkmoths,
1577                             Jepson Online    butterfly    hawkmoths,
1578                             Jepson Online    butterfly    hawkmoths,
1579                             Jepson Online    butterfly    hawkmoths,
1580                             Jepson Online    butterfly    hawkmoths,
1581                             Jepson Online    butterfly    hawkmoths,
1582                             Jepson Online         <NA>          <NA>
1583                             Jepson Online         <NA>          <NA>
1584                             Jepson Online         <NA>          <NA>
1585                             Jepson Online         <NA>          <NA>
1586                             Jepson Online         <NA>          <NA>
1587                             Jepson Online         <NA>          <NA>
1588                             Jepson Online         <NA>          <NA>
1589                             Jepson Online         <NA>          <NA>
1590                             Jepson Online         <NA>          <NA>
1591                             Jepson Online         <NA>          <NA>
1592                             Jepson Online         <NA>          <NA>
1593                             Jepson Online         <NA>          <NA>
1594                             Jepson Online         <NA>          <NA>
1595                             Jepson Online         <NA>          <NA>
1596                             Jepson Online         <NA>          <NA>
1597                             Jepson Online         <NA>          <NA>
1598                             Jepson Online         <NA>          <NA>
1599                             Jepson Online         <NA>          <NA>
1600                             Jepson Online         <NA>          <NA>
1601                             Jepson Online         <NA>          <NA>
1602                             Jepson Online         <NA>          <NA>
1603                             Jepson Online         <NA>          <NA>
1604                             Jepson Online         <NA>          <NA>
1605                             Jepson Online         <NA>          <NA>
1606                             Jepson Online         <NA>          <NA>
1607                             Jepson Online         <NA>          <NA>
1608                             Jepson Online         <NA>          <NA>
1609                             Jepson Online          bee       beetles
1610                             Jepson Online          bee       beetles
1611                             Jepson Online          bee       beetles
1612                             Jepson Online          bee       beetles
1613                             Jepson Online          bee       beetles
1614                             Jepson Online          bee       beetles
1615                             Jepson Online          bee       beetles
1616                             Jepson Online          bee       beetles
1617                             Jepson Online          bee       beetles
1618                             Jepson Online          bee       beetles
1619                             Jepson Online     hawkmoth          <NA>
1620                             Jepson Online     hawkmoth          <NA>
1621                             Jepson Online     hawkmoth          <NA>
1622                             Jepson Online     hawkmoth          <NA>
1623                             Jepson Online     hawkmoth          <NA>
1624                             Jepson Online     hawkmoth          <NA>
1625                             Jepson Online     hawkmoth          <NA>
1626                             Jepson Online     hawkmoth          <NA>
1627                             Jepson Online     hawkmoth          <NA>
1628                             Jepson Online     hawkmoth          <NA>
1629                             Jepson Online     hawkmoth          <NA>
1630                             Jepson Online     hawkmoth          <NA>
1631                             Jepson Online     hawkmoth          <NA>
1632                             Jepson Online         <NA>          <NA>
1633                             Jepson Online         <NA>          <NA>
1634                             Jepson Online         <NA>          <NA>
1635                             Jepson Online         <NA>          <NA>
1636                             Jepson Online         <NA>          <NA>
1637                             Jepson Online         <NA>          <NA>
1638                             Jepson Online         <NA>          <NA>
1639                             Jepson Online         <NA>          <NA>
1640                             Jepson Online         <NA>          <NA>
1641                             Jepson Online         <NA>          <NA>
1642                             Jepson Online         <NA>          <NA>
1643                             Jepson Online         <NA>          <NA>
1644                             Jepson Online         <NA>          <NA>
1645                             Jepson Online         <NA>          <NA>
1646                             Jepson Online         <NA>          <NA>
1647                             Jepson Online         <NA>          <NA>
1648                             Jepson Online         <NA>          <NA>
1649                             Jepson Online         <NA>          <NA>
1650                             Jepson Online         <NA>          <NA>
1651                             Jepson Online         <NA>          <NA>
1652                             Jepson Online         <NA>          <NA>
1653                             Jepson Online         <NA>          <NA>
1654                             Jepson Online         <NA>          <NA>
1655                             Jepson Online         <NA>          <NA>
1656                             Jepson Online         <NA>          <NA>
1657                             Jepson Online         <NA>          <NA>
1658                             Jepson Online         <NA>          <NA>
1659                             Jepson Online         <NA>          <NA>
1660                             Jepson Online         <NA>          <NA>
1661                             Jepson Online         <NA>          <NA>
1662                             Jepson Online         <NA>          <NA>
1663                             Jepson Online         <NA>          <NA>
1664                             Jepson Online         <NA>          <NA>
1665                             Jepson Online         <NA>          <NA>
1666                             Jepson Online         <NA>          <NA>
1667                             Jepson Online         <NA>          <NA>
1668                             Jepson Online         <NA>          <NA>
1669                             Jepson Online         <NA>          <NA>
1670                             Jepson Online         <NA>          <NA>
1671                             Jepson Online         <NA>          <NA>
1672                             Jepson Online       beetle          <NA>
1673                             Jepson Online       beetle          <NA>
1674                             Jepson Online       beetle          <NA>
1675                             Jepson Online       beetle          <NA>
1676                             Jepson Online       beetle          <NA>
1677                             Jepson Online       beetle          <NA>
1678                             Jepson Online       beetle          <NA>
1679                             Jepson Online         <NA>          <NA>
1680                             Jepson Online         <NA>          <NA>
1681                             Jepson Online         <NA>          <NA>
1682                             Jepson Online         <NA>          <NA>
1683                             Jepson Online         <NA>          <NA>
1684                             Jepson Online         <NA>          <NA>
1685                             Jepson Online         <NA>          <NA>
1686                             Jepson Online         <NA>          <NA>
1687                             Jepson Online         <NA>          <NA>
1688                             Jepson Online         <NA>          <NA>
1689                   Cal Academy of Sciences         <NA>          <NA>
1690                   Cal Academy of Sciences         <NA>          <NA>
1691                   Cal Academy of Sciences         <NA>          <NA>
1692                   Cal Academy of Sciences         <NA>          <NA>
1693        Wyoming Natural Diversity Database         <NA>          <NA>
1694        Wyoming Natural Diversity Database         <NA>          <NA>
1695        Wyoming Natural Diversity Database         <NA>          <NA>
1696        Wyoming Natural Diversity Database         <NA>          <NA>
1697        Wyoming Natural Diversity Database         <NA>          <NA>
1698        Wyoming Natural Diversity Database         <NA>          <NA>
1699        Wyoming Natural Diversity Database         <NA>          <NA>
1700        Wyoming Natural Diversity Database         <NA>          <NA>
1701        Wyoming Natural Diversity Database         <NA>          <NA>
1702        Wyoming Natural Diversity Database         <NA>          <NA>
1703        Wyoming Natural Diversity Database         <NA>          <NA>
1704        Wyoming Natural Diversity Database         <NA>          <NA>
1705        Wyoming Natural Diversity Database         <NA>          <NA>
1706                 Herbarium sheet MO4074472         <NA>          <NA>
1707                 Herbarium sheet MO4074472         <NA>          <NA>
1708                 Herbarium sheet MO4074472         <NA>          <NA>
1709                 Herbarium sheet MO4074472         <NA>          <NA>
1710                 Herbarium sheet MO4074472         <NA>          <NA>
1711                 Herbarium sheet MO4074472         <NA>          <NA>
1712                 Herbarium sheet MO4074472         <NA>          <NA>
1713                Herbarium sheet FLAS216787         <NA>          <NA>
1714                Herbarium sheet FLAS216787         <NA>          <NA>
1715                Herbarium sheet FLAS216787         <NA>          <NA>
1716                Herbarium sheet FLAS216787         <NA>          <NA>
1717                Herbarium sheet FLAS216787         <NA>          <NA>
1718                Herbarium sheet FLAS216787         <NA>          <NA>
1719                Herbarium sheet FLAS216787         <NA>          <NA>
1720                Herbarium sheet FLAS216787         <NA>          <NA>
1721                Herbarium sheet FLAS216787         <NA>          <NA>
1722                                   Conabio         <NA>          <NA>
1723                                   Conabio         <NA>          <NA>
1724                                   Conabio         <NA>          <NA>
1725                                   Conabio         <NA>          <NA>
1726                                   Conabio         <NA>          <NA>
1727                                   Conabio         <NA>          <NA>
1728                                   Conabio         <NA>          <NA>
1729                                   Conabio         <NA>          <NA>
1730                                   Conabio         <NA>          <NA>
1731                                   Conabio         <NA>          <NA>
1732                                   Conabio         <NA>          <NA>
1733                                   Conabio         <NA>          <NA>
1734                                   Conabio         <NA>          <NA>
1735                                   Conabio         <NA>          <NA>
1736                                   Conabio         <NA>          <NA>
1737                                   Conabio         <NA>          <NA>
1738                                   Conabio         <NA>          <NA>
1739                         Grant  Grant 1965         <NA>          <NA>
1740                         Grant  Grant 1965         <NA>          <NA>
1741                         Grant  Grant 1965         <NA>          <NA>
1742                         Grant  Grant 1965         <NA>          <NA>
1743                         Grant  Grant 1965         <NA>          <NA>
1744                         Grant  Grant 1965         <NA>          <NA>
1745                         Grant  Grant 1965         <NA>          <NA>
1746                                   Conabio         <NA>          <NA>
1747                                   Conabio         <NA>          <NA>
1748                                   Conabio         <NA>          <NA>
1749                                   Conabio         <NA>          <NA>
1750                                   Conabio         <NA>          <NA>
1751                                   Conabio         <NA>          <NA>
1752                                   Conabio         <NA>          <NA>
1753                                   Conabio         <NA>          <NA>
1754                                   Conabio         <NA>          <NA>
1755                                   Conabio         <NA>          <NA>
1756                                   Conabio         <NA>          <NA>
1757                                   Conabio         <NA>          <NA>
1758                                   Conabio         <NA>          <NA>
1759                                   Conabio         <NA>          <NA>
1760                                   Conabio         <NA>          <NA>
1761                                   Conabio         <NA>          <NA>
1762                                   Conabio         <NA>          <NA>
1763                                   Conabio         <NA>          <NA>
1764                                   Conabio         <NA>          <NA>
1765                                   Conabio         <NA>          <NA>
1766                                   Conabio         <NA>          <NA>
1767                                   Conabio         <NA>          <NA>
1768                                   Conabio         <NA>          <NA>
1769                                   Conabio         <NA>          <NA>
1770                                   Conabio         <NA>          <NA>
1771                         Grant  Grant 1965  hummingbird          <NA>
1772                         Grant  Grant 1965  hummingbird          <NA>
1773                         Grant  Grant 1965  hummingbird          <NA>
1774                         Grant  Grant 1965  hummingbird          <NA>
1775                                   Conabio         <NA>          <NA>
1776                                   Conabio         <NA>          <NA>
1777                                   Conabio         <NA>          <NA>
1778                                   Conabio         <NA>          <NA>
1779                                   Conabio         <NA>          <NA>
1780                                   Conabio         <NA>          <NA>
1781                                   Conabio         <NA>          <NA>
1782                                   Conabio         <NA>          <NA>
1783                                   Conabio         <NA>          <NA>
1784                                   Conabio         <NA>          <NA>
1785                                   Conabio         <NA>          <NA>
1786                                   Conabio         <NA>          <NA>
1787                                   Conabio         <NA>          <NA>
1788                                   Conabio         <NA>          <NA>
1789                                   Conabio         <NA>          <NA>
1790                                   Conabio         <NA>          <NA>
1791                                   Conabio         <NA>          <NA>
1792                                   Conabio         <NA>          <NA>
1793                                   Conabio         <NA>          <NA>
1794                                   Conabio         <NA>          <NA>
1795                                   Conabio         <NA>          <NA>
1796                                   Conabio         <NA>          <NA>
1797                                   Conabio         <NA>          <NA>
1798               Herbarium sheet RSABG692006         <NA>          <NA>
1799               Herbarium sheet RSABG692006         <NA>          <NA>
1800               Herbarium sheet RSABG692006         <NA>          <NA>
1801               Herbarium sheet RSABG692006         <NA>          <NA>
1802               Herbarium sheet RSABG688743         <NA>          <NA>
1803               Herbarium sheet RSABG688743         <NA>          <NA>
1804               Herbarium sheet RSABG688743         <NA>          <NA>
1805               Herbarium sheet RSABG688738         <NA>          <NA>
1806               Herbarium sheet RSABG688738         <NA>          <NA>
1807               Herbarium sheet RSABG688738         <NA>          <NA>
1808               Herbarium sheet RSABG688738         <NA>          <NA>
1809               Herbarium sheet RSABG688738         <NA>          <NA>
1810                             Jepson Online         <NA>          <NA>
1811                             Jepson Online         <NA>          <NA>
1812                             Jepson Online         <NA>          <NA>
1813                             Jepson Online         <NA>          <NA>
1814                             Jepson Online         <NA>          <NA>
1815                             Jepson Online         <NA>          <NA>
1816                             Jepson Online         <NA>          <NA>
1817                         Grant  Grant 1965      bee-fly           bee
1818                         Grant  Grant 1965      bee-fly           bee
1819                         Grant  Grant 1965      bee-fly           bee
1820                         Grant  Grant 1965      bee-fly           bee
1821                         Grant  Grant 1965      bee-fly           bee
1822                         Grant  Grant 1965      bee-fly           bee
1823                         Grant  Grant 1965      bee-fly           bee
1824                         Grant  Grant 1965      bee-fly           bee
1825                         Grant  Grant 1965      bee-fly           bee
1826                         Grant  Grant 1965      bee-fly           bee
1827                         Grant  Grant 1965      bee-fly           bee
1828                         Grant  Grant 1965      bee-fly           bee
1829                         Grant  Grant 1965      bee-fly           bee
1830                         Grant  Grant 1965      bee-fly           bee
1831                         Grant  Grant 1965      bee-fly           bee
1832                         Grant  Grant 1965      bee-fly           bee
1833                             Jepson Online         <NA>          <NA>
1834                             Jepson Online         <NA>          <NA>
1835                             Jepson Online         <NA>          <NA>
1836                             Jepson Online         <NA>          <NA>
1837                             Jepson Online         <NA>          <NA>
1838                             Jepson Online         <NA>          <NA>
1839                             Jepson Online         <NA>          <NA>
1840                             Jepson Online         <NA>          <NA>
1841                             Jepson Online         <NA>          <NA>
1842           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1843           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1844           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1845           Thomas Juenger Lab, Univ. Texas         <NA>          <NA>
1846                             Locklear 2011         <NA>          <NA>
1847                             Locklear 2011         <NA>          <NA>
1848                             Locklear 2011         <NA>          <NA>
1849                             Locklear 2011         <NA>          <NA>
1850                             Locklear 2011         <NA>          <NA>
1851                             Locklear 2011         <NA>          <NA>
1852                             Locklear 2011         <NA>          <NA>
1853                             Locklear 2011         <NA>          <NA>
1854                             Locklear 2011         <NA>          <NA>
1855                             Locklear 2011         <NA>          <NA>
1856                             Locklear 2011         <NA>          <NA>
1857                             Locklear 2011         <NA>          <NA>
1858                             Locklear 2011         <NA>          <NA>
1859                             Locklear 2011         <NA>          <NA>
1860                             Locklear 2011         <NA>          <NA>
1861                         Grant  Grant 1965          bee       bee-fly
1862                         Grant  Grant 1965          bee       bee-fly
1863                         Grant  Grant 1965          bee       bee-fly
1864                         Grant  Grant 1965          bee       bee-fly
1865                             Jepson Online         <NA>          <NA>
1866                             Jepson Online         <NA>          <NA>
1867                             Jepson Online         <NA>          <NA>
1868                             Jepson Online         <NA>          <NA>
1869                             Jepson Online         <NA>          <NA>
1870                             Jepson Online         <NA>          <NA>
1871                             Jepson Online         <NA>          <NA>
1872                             Jepson Online         <NA>          <NA>
1873                             Jepson Online         <NA>          <NA>
1874                             Jepson Online         <NA>          <NA>
1875                             Jepson Online         <NA>          <NA>
1876                             Jepson Online         <NA>          <NA>
1877                             Jepson Online         <NA>          <NA>
1878                             Jepson Online         <NA>          <NA>
1879                             Jepson Online         <NA>          <NA>
1880                             Jepson Online         <NA>          <NA>
1881                             Jepson Online         <NA>          <NA>
1882                             Jepson Online         <NA>          <NA>
1883                             Jepson Online         <NA>          <NA>
1884                             Jepson Online         <NA>          <NA>
1885                             Jepson Online         <NA>          <NA>
1886                             Jepson Online         <NA>          <NA>
1887                             Jepson Online         <NA>          <NA>
1888                             Jepson Online         <NA>          <NA>
1889                             Jepson Online         <NA>          <NA>
1890                             Jepson Online         <NA>          <NA>
1891                             Jepson Online         <NA>          <NA>
1892                             Jepson Online         <NA>          <NA>
1893                         Grant  Grant 1965      bee-fly           bee
1894                         Grant  Grant 1965      bee-fly           bee
1895                         Grant  Grant 1965      bee-fly           bee
1896                             Jepson Online         <NA>          <NA>
1897                             Jepson Online         <NA>          <NA>
1898                             Jepson Online         <NA>          <NA>
1899                             Jepson Online         <NA>          <NA>
1900                             Jepson Online         <NA>          <NA>
1901                             Jepson Online         <NA>          <NA>
1902                             Jepson Online         <NA>          <NA>
1903                             Jepson Online         <NA>          <NA>
1904                             Jepson Online         <NA>          <NA>
1905                             Jepson Online         <NA>          <NA>
1906                             Jepson Online         <NA>          <NA>
1907                             Jepson Online         <NA>          <NA>
1908                             Jepson Online         <NA>          <NA>
1909                             Jepson Online         <NA>          <NA>
1910                             Jepson Online         <NA>          <NA>
1911                             Jepson Online         <NA>          <NA>
1912                             Jepson Online         <NA>          <NA>
1913                         Grant  Grant 1965          bee       bee-fly
1914                         Grant  Grant 1965          bee       bee-fly
1915                         Grant  Grant 1965          bee       bee-fly
1916                         Grant  Grant 1965          bee       bee-fly
1917                         Grant  Grant 1965          bee       bee-fly
1918                         Grant  Grant 1965          bee       bee-fly
1919                         Grant  Grant 1965          bee       bee-fly
1920                         Grant  Grant 1965          bee       bee-fly
1921                         Grant  Grant 1965          bee       bee-fly
1922                         Grant  Grant 1965          bee       bee-fly
1923                         Grant  Grant 1965          bee       bee-fly
1924                         Grant  Grant 1965          bee       bee-fly
1925                         Grant  Grant 1965          bee       bee-fly
1926                                 CalPhotos         <NA>          <NA>
1927                                 CalPhotos         <NA>          <NA>
1928                                 CalPhotos         <NA>          <NA>
1929                                 CalPhotos         <NA>          <NA>
1930                             Jepson Online      bee-fly          <NA>
1931                             Jepson Online      bee-fly          <NA>
1932                             Jepson Online      bee-fly          <NA>
1933                             Jepson Online      bee-fly          <NA>
1934                         Grant  Grant 1965          bee          <NA>
1935                         Grant  Grant 1965          bee          <NA>
1936                         Grant  Grant 1965          bee          <NA>
1937                         Grant  Grant 1965          bee          <NA>
1938                         Grant  Grant 1965          bee          <NA>
1939                         Grant  Grant 1965          bee          <NA>
1940                         Grant  Grant 1965          bee          <NA>
1941                             Jepson Online         <NA>          <NA>
1942                             Jepson Online         <NA>          <NA>
1943                             Jepson Online         <NA>          <NA>
1944                             Jepson Online         <NA>          <NA>
1945                              Johnson 2012         <NA>          <NA>
1946                             Jepson Online         <NA>          <NA>
1947                             Jepson Online         <NA>          <NA>
1948                             Jepson Online         <NA>          <NA>
1949                             Jepson Online         <NA>          <NA>
1950                             Jepson Online         <NA>          <NA>
1951                             Jepson Online         <NA>          <NA>
1952                             Jepson Online         <NA>          <NA>
1953                             Jepson Online         <NA>          <NA>
1954                             Jepson Online         <NA>          <NA>
1955                             Jepson Online         <NA>          <NA>
1956                         Grant  Grant 1965         <NA>          <NA>
1957                         Grant  Grant 1965         <NA>          <NA>
1958                         Grant  Grant 1965         <NA>          <NA>
1959                         Grant  Grant 1965         <NA>          <NA>
1960                         Grant  Grant 1965         <NA>          <NA>
1961                         Grant  Grant 1965         <NA>          <NA>
1962                         Grant  Grant 1965         <NA>          <NA>
1963                         Grant  Grant 1965         <NA>          <NA>
1964                         Grant  Grant 1965         <NA>          <NA>
1965                         Grant  Grant 1965         <NA>          <NA>
1966                         Grant  Grant 1965         <NA>          <NA>
1967                         Grant  Grant 1965         <NA>          <NA>
1968                             Jepson Online         <NA>          <NA>
1969                             Jepson Online         <NA>          <NA>
1970                             Jepson Online         <NA>          <NA>
1971                             Jepson Online         <NA>          <NA>
1972                             Jepson Online         <NA>          <NA>
1973                             Jepson Online         <NA>          <NA>
1974                             Jepson Online         <NA>          <NA>
1975                             Jepson Online         <NA>          <NA>
1976                             Jepson Online         <NA>          <NA>
1977                             Jepson Online         <NA>          <NA>
1978                             Jepson Online         <NA>          <NA>
1979                             Jepson Online         <NA>          <NA>
1980                         Grant  Grant 1965          bee          <NA>
1981                         Grant  Grant 1965          bee          <NA>
1982                         Grant  Grant 1965          bee          <NA>
1983                         Grant  Grant 1965          bee          <NA>
1984                         Grant  Grant 1965          bee          <NA>
1985                         Grant  Grant 1965          bee          <NA>
1986                         Grant  Grant 1965          bee          <NA>
1987                             Locklear 2011         <NA>          <NA>
1988                             Locklear 2011         <NA>          <NA>
1989                             Locklear 2011         <NA>          <NA>
1990                             Locklear 2011         <NA>          <NA>
1991                             Locklear 2011         <NA>          <NA>
1992                             Locklear 2011         <NA>          <NA>
1993                             Locklear 2011         <NA>          <NA>
1994                             Locklear 2011         <NA>          <NA>
1995                             Locklear 2011         <NA>          <NA>
1996                             Locklear 2011         <NA>          <NA>
1997                             Locklear 2011         <NA>          <NA>
1998                             Locklear 2011         <NA>          <NA>
1999                             Locklear 2011         <NA>          <NA>
2000                             Locklear 2011         <NA>          <NA>
2001                             Locklear 2011         <NA>          <NA>
2002                             Locklear 2011         <NA>          <NA>
2003                             Locklear 2011         <NA>          <NA>
2004                             Locklear 2011         <NA>          <NA>
2005                             Locklear 2011         <NA>          <NA>
2006                             Locklear 2011         <NA>          <NA>
2007                             Locklear 2011         <NA>          <NA>
2008                             Locklear 2011         <NA>          <NA>
2009                             Locklear 2011         <NA>          <NA>
2010                             Locklear 2011         <NA>          <NA>
2011                             Locklear 2011         <NA>          <NA>
2012                             Locklear 2011         <NA>          <NA>
2013                             Locklear 2011         <NA>          <NA>
2014                             Locklear 2011         <NA>          <NA>
2015                             Locklear 2011         <NA>          <NA>
2016                             Locklear 2011         <NA>          <NA>
2017                             Locklear 2011         <NA>          <NA>
2018                             Locklear 2011         <NA>          <NA>
2019                             Locklear 2011         <NA>          <NA>
2020                             Locklear 2011         <NA>          <NA>
2021                             Locklear 2011         <NA>          <NA>
2022                             Locklear 2011         <NA>          <NA>
2023                             Locklear 2011         <NA>          <NA>
2024                             Locklear 2011         <NA>          <NA>
2025                             Locklear 2011         <NA>          <NA>
2026                             Locklear 2011         <NA>          <NA>
2027                             Locklear 2011         <NA>          <NA>
2028                             Locklear 2011         <NA>          <NA>
2029                             Locklear 2011    butterfly          <NA>
2030                             Locklear 2011    butterfly          <NA>
2031                             Locklear 2011    butterfly          <NA>
2032                             Locklear 2011    butterfly          <NA>
2033                             Locklear 2011    butterfly          <NA>
2034                             Locklear 2011    butterfly          <NA>
2035                             Locklear 2011    butterfly          <NA>
2036                             Locklear 2011    butterfly          <NA>
2037                             Locklear 2011    butterfly          <NA>
2038                             Locklear 2011    butterfly          <NA>
2039                             Locklear 2011         <NA>          <NA>
2040                             Locklear 2011         <NA>          <NA>
2041                             Locklear 2011         <NA>          <NA>
2042                             Locklear 2011         <NA>          <NA>
2043                             Locklear 2011         <NA>          <NA>
2044                             Locklear 2011         <NA>          <NA>
2045                             Locklear 2011         <NA>          <NA>
2046                             Locklear 2011         <NA>          <NA>
2047                             Locklear 2011         <NA>          <NA>
2048                             Locklear 2011         <NA>          <NA>
2049                             Locklear 2011         <NA>          <NA>
2050                             Locklear 2011         <NA>          <NA>
2051                             Locklear 2011         <NA>          <NA>
2052                             Locklear 2011         <NA>          <NA>
2053                             Locklear 2011         <NA>          <NA>
2054                             Locklear 2011         <NA>          <NA>
2055                             Locklear 2011         <NA>          <NA>
2056                             Locklear 2011         <NA>          <NA>
2057                             Locklear 2011         <NA>          <NA>
2058                             Locklear 2011         <NA>          <NA>
2059                             Locklear 2011         <NA>          <NA>
2060                             Locklear 2011         <NA>          <NA>
2061                             Locklear 2011         <NA>          <NA>
2062                             Locklear 2011         <NA>          <NA>
2063                             Locklear 2011     hawkmoth          <NA>
2064                             Locklear 2011     hawkmoth          <NA>
2065                             Locklear 2011     hawkmoth          <NA>
2066                             Locklear 2011     hawkmoth          <NA>
2067                             Locklear 2011     hawkmoth          <NA>
2068                             Locklear 2011     hawkmoth          <NA>
2069                             Locklear 2011     hawkmoth          <NA>
2070                             Locklear 2011         <NA>          <NA>
2071                             Locklear 2011         <NA>          <NA>
2072                             Locklear 2011         <NA>          <NA>
2073                             Locklear 2011         <NA>          <NA>
2074                             Locklear 2011         <NA>          <NA>
2075                             Locklear 2011         <NA>          <NA>
2076                             Locklear 2011         <NA>          <NA>
2077                             Locklear 2011         <NA>          <NA>
2078                             Locklear 2011         <NA>          <NA>
2079                             Locklear 2011         <NA>          <NA>
2080                             Locklear 2011         <NA>          <NA>
2081                             Locklear 2011         <NA>          <NA>
2082                             Locklear 2011         <NA>          <NA>
2083                             Locklear 2011         <NA>          <NA>
2084                             Locklear 2011         <NA>          <NA>
2085                             Locklear 2011         <NA>          <NA>
2086                             Locklear 2011         <NA>          <NA>
2087                             Locklear 2011         <NA>          <NA>
2088                             Locklear 2011         <NA>          <NA>
2089                             Locklear 2011         <NA>          <NA>
2090                             Locklear 2011         <NA>          <NA>
2091                             Locklear 2011         <NA>          <NA>
2092                             Locklear 2011         <NA>          <NA>
2093                             Locklear 2011         <NA>          <NA>
2094                             Locklear 2011         <NA>          <NA>
2095                             Locklear 2011         <NA>          <NA>
2096                             Locklear 2011         <NA>          <NA>
2097                             Locklear 2011         <NA>          <NA>
2098                             Locklear 2011         <NA>          <NA>
2099                             Locklear 2011         <NA>          <NA>
2100                             Locklear 2011         <NA>          <NA>
2101                             Locklear 2011         <NA>          <NA>
2102                             Locklear 2011         <NA>          <NA>
2103                             Locklear 2011         <NA>          <NA>
2104                             Locklear 2011         <NA>          <NA>
2105                             Locklear 2011         <NA>          <NA>
2106                             Locklear 2011         <NA>          <NA>
2107                             Locklear 2011         <NA>          <NA>
2108                             Locklear 2011         <NA>          <NA>
2109                             Locklear 2011         <NA>          <NA>
2110                             Locklear 2011     hawkmoth     butterfly
2111                             Locklear 2011     hawkmoth     butterfly
2112                             Locklear 2011     hawkmoth     butterfly
2113                             Locklear 2011     hawkmoth     butterfly
2114                             Locklear 2011     hawkmoth     butterfly
2115                             Locklear 2011     hawkmoth     butterfly
2116                             Locklear 2011     hawkmoth     butterfly
2117                             Locklear 2011     hawkmoth     butterfly
2118                             Locklear 2011     hawkmoth     butterfly
2119                             Locklear 2011     hawkmoth     butterfly
2120                             Locklear 2011         <NA>          <NA>
2121                             Locklear 2011         <NA>          <NA>
2122                             Locklear 2011         <NA>          <NA>
2123                             Locklear 2011         <NA>          <NA>
2124                             Locklear 2011         <NA>          <NA>
2125                             Locklear 2011    butterfly          <NA>
2126                             Locklear 2011    butterfly          <NA>
2127                             Locklear 2011    butterfly          <NA>
2128                             Locklear 2011    butterfly          <NA>
2129                             Locklear 2011    butterfly          <NA>
2130                             Locklear 2011    butterfly          <NA>
2131                             Locklear 2011    butterfly          <NA>
2132                             Locklear 2011    butterfly          <NA>
2133                             Locklear 2011    butterfly          <NA>
2134                             Locklear 2011    butterfly          <NA>
2135                             Locklear 2011    butterfly          <NA>
2136                             Locklear 2011    butterfly          <NA>
2137                             Locklear 2011    butterfly          <NA>
2138                             Locklear 2011    butterfly          <NA>
2139                             Locklear 2011    butterfly          <NA>
2140                             Locklear 2011    butterfly          <NA>
2141                             Locklear 2011    butterfly          <NA>
2142                             Locklear 2011    butterfly          <NA>
2143                             Locklear 2011    butterfly          <NA>
2144                             Locklear 2011    butterfly          <NA>
2145                             Locklear 2011    butterfly          <NA>
2146                             Locklear 2011    butterfly          <NA>
2147                             Locklear 2011    butterfly          <NA>
2148                             Locklear 2011    butterfly          <NA>
2149                             Locklear 2011     hawkmoth          <NA>
2150                             Locklear 2011     hawkmoth          <NA>
2151                             Locklear 2011     hawkmoth          <NA>
2152                             Locklear 2011     hawkmoth          <NA>
2153                             Locklear 2011     hawkmoth          <NA>
2154                             Locklear 2011     hawkmoth          <NA>
2155                             Locklear 2011     hawkmoth          <NA>
2156                             Locklear 2011         <NA>          <NA>
2157                             Locklear 2011         <NA>          <NA>
2158                             Locklear 2011         <NA>          <NA>
2159                             Locklear 2011         <NA>          <NA>
2160                             Locklear 2011         <NA>          <NA>
2161                             Locklear 2011         <NA>          <NA>
2162                             Locklear 2011         <NA>          <NA>
2163                             Locklear 2011         <NA>          <NA>
2164                             Locklear 2011         <NA>          <NA>
2165                             Locklear 2011         <NA>          <NA>
2166                             Locklear 2011         <NA>          <NA>
2167                             Locklear 2011         <NA>          <NA>
2168                             Locklear 2011         <NA>          <NA>
2169                             Locklear 2011         <NA>          <NA>
2170                             Locklear 2011         <NA>          <NA>
2171                             Locklear 2011         <NA>          <NA>
2172                             Locklear 2011         <NA>          <NA>
2173                             Locklear 2011         <NA>          <NA>
2174                             Locklear 2011         <NA>          <NA>
2175                             Locklear 2011         <NA>          <NA>
2176                             Locklear 2011         <NA>          <NA>
2177                             Locklear 2011         <NA>          <NA>
2178                             Locklear 2011         <NA>          <NA>
2179                             Locklear 2011         <NA>          <NA>
2180                             Locklear 2011         <NA>          <NA>
2181                             Locklear 2011         <NA>          <NA>
2182                             Locklear 2011         <NA>          <NA>
2183                             Locklear 2011         <NA>          <NA>
2184                             Locklear 2011         <NA>          <NA>
2185                             Locklear 2011         <NA>          <NA>
2186                             Locklear 2011         <NA>          <NA>
2187                             Locklear 2011         <NA>          <NA>
2188                             Locklear 2011         <NA>          <NA>
2189                             Locklear 2011         <NA>          <NA>
2190                             Locklear 2011         <NA>          <NA>
2191                             Locklear 2011         <NA>          <NA>
2192                             Locklear 2011         <NA>          <NA>
2193                             Locklear 2011         <NA>          <NA>
2194                             Locklear 2011         <NA>          <NA>
2195                             Locklear 2011         <NA>          <NA>
2196                             Locklear 2011         <NA>          <NA>
2197                             Locklear 2011         <NA>          <NA>
2198                             Locklear 2011         <NA>          <NA>
2199                             Locklear 2011         <NA>          <NA>
2200                             Locklear 2011         <NA>          <NA>
2201                             Locklear 2011         <NA>          <NA>
2202                             Locklear 2011    butterfly          <NA>
2203                             Locklear 2011    butterfly          <NA>
2204                             Locklear 2011    butterfly          <NA>
2205                             Locklear 2011    butterfly          <NA>
2206                             Locklear 2011    butterfly          <NA>
2207                             Locklear 2011    butterfly          <NA>
2208                             Locklear 2011    butterfly          <NA>
2209                             Locklear 2011    butterfly          <NA>
2210                             Locklear 2011    butterfly          <NA>
2211                             Locklear 2011    butterfly          <NA>
2212                             Locklear 2011    butterfly          <NA>
2213                             Locklear 2011    butterfly          <NA>
2214                             Locklear 2011    butterfly          <NA>
2215                             Locklear 2011    butterfly          <NA>
2216                             Locklear 2011    butterfly          <NA>
2217                             Locklear 2011    butterfly          <NA>
2218                             Locklear 2011         <NA>          <NA>
2219                             Locklear 2011         <NA>          <NA>
2220                             Locklear 2011         <NA>          <NA>
2221                             Locklear 2011         <NA>          <NA>
2222                             Locklear 2011         <NA>          <NA>
2223                             Locklear 2011         <NA>          <NA>
2224                             Locklear 2011         <NA>          <NA>
2225                             Locklear 2011         <NA>          <NA>
2226                             Locklear 2011         <NA>          <NA>
2227                             Locklear 2011         <NA>          <NA>
2228                             Locklear 2011         <NA>          <NA>
2229                             Locklear 2011         <NA>          <NA>
2230                             Locklear 2011         <NA>          <NA>
2231                             Locklear 2011         <NA>          <NA>
2232                             Locklear 2011         <NA>          <NA>
2233                             Locklear 2011    butterfly          <NA>
2234                             Locklear 2011    butterfly          <NA>
2235                             Locklear 2011    butterfly          <NA>
2236                             Locklear 2011    butterfly          <NA>
2237                             Locklear 2011    butterfly          <NA>
2238                             Locklear 2011    butterfly          <NA>
2239                             Locklear 2011    butterfly          <NA>
2240                             Locklear 2011    butterfly          <NA>
2241                             Locklear 2011    butterfly          <NA>
2242                             Locklear 2011    butterfly          <NA>
2243                             Locklear 2011    butterfly          <NA>
2244                             Locklear 2011    butterfly          <NA>
2245                             Locklear 2011    butterfly          <NA>
2246                             Locklear 2011    butterfly          <NA>
2247                             Locklear 2011    butterfly          <NA>
2248                             Locklear 2011    butterfly          <NA>
2249                             Locklear 2011    butterfly          <NA>
2250                             Locklear 2011    butterfly          <NA>
2251                             Locklear 2011    butterfly          <NA>
2252                             Locklear 2011    butterfly          <NA>
2253                             Locklear 2011    butterfly          <NA>
2254                             Locklear 2011    butterfly          <NA>
2255                             Locklear 2011    butterfly          <NA>
2256                             Locklear 2011    butterfly          <NA>
2257                             Locklear 2011    butterfly          <NA>
2258                             Locklear 2011    butterfly          <NA>
2259                             Locklear 2011         <NA>          <NA>
2260                             Locklear 2011         <NA>          <NA>
2261                             Locklear 2011         <NA>          <NA>
2262                             Locklear 2011         <NA>          <NA>
2263                             Locklear 2011         <NA>          <NA>
2264                             Locklear 2011         <NA>          <NA>
2265                             Locklear 2011         <NA>          <NA>
2266                             Locklear 2011         <NA>          <NA>
2267                             Locklear 2011         <NA>          <NA>
2268                             Locklear 2011         <NA>          <NA>
2269                             Locklear 2011    butterfly          <NA>
2270                             Locklear 2011    butterfly          <NA>
2271                             Locklear 2011    butterfly          <NA>
2272                             Locklear 2011    butterfly          <NA>
2273                             Locklear 2011    butterfly          <NA>
2274                             Locklear 2011    butterfly          <NA>
2275                             Locklear 2011    butterfly          <NA>
2276                             Locklear 2011    butterfly          <NA>
2277                             Locklear 2011    butterfly          <NA>
2278                             Locklear 2011    butterfly          <NA>
2279                             Locklear 2011         <NA>          <NA>
2280                             Locklear 2011         <NA>          <NA>
2281                             Locklear 2011         <NA>          <NA>
2282                             Locklear 2011         <NA>          <NA>
2283                             Locklear 2011         <NA>          <NA>
2284                             Locklear 2011         <NA>          <NA>
2285                             Locklear 2011         <NA>          <NA>
2286                             Locklear 2011         <NA>          <NA>
2287                             Locklear 2011         <NA>          <NA>
2288                             Locklear 2011         <NA>          <NA>
2289                             Locklear 2011         <NA>          <NA>
2290                             Locklear 2011         <NA>          <NA>
2291                             Locklear 2011         <NA>          <NA>
2292                             Locklear 2011         <NA>          <NA>
2293                             Locklear 2011         <NA>          <NA>
2294                             Locklear 2011         <NA>          <NA>
2295                             Locklear 2011         <NA>          <NA>
2296                             Locklear 2011         <NA>          <NA>
2297                             Locklear 2011         <NA>          <NA>
2298                             Locklear 2011         <NA>          <NA>
2299                             Locklear 2011         <NA>          <NA>
2300                             Locklear 2011         <NA>          <NA>
2301                             Locklear 2011         <NA>          <NA>
2302                             Locklear 2011         <NA>          <NA>
2303                             Locklear 2011         <NA>          <NA>
2304                             Locklear 2011         <NA>          <NA>
2305                             Locklear 2011         <NA>          <NA>
2306                             Locklear 2011         <NA>          <NA>
2307                             Locklear 2011         <NA>          <NA>
2308                             Locklear 2011         <NA>          <NA>
2309                             Locklear 2011         <NA>          <NA>
2310                             Locklear 2011         <NA>          <NA>
2311                             Locklear 2011         <NA>          <NA>
2312                             Locklear 2011         <NA>          <NA>
2313                             Locklear 2011         <NA>          <NA>
2314                             Locklear 2011         <NA>          <NA>
2315                             Locklear 2011         <NA>          <NA>
2316                             Locklear 2011         <NA>          <NA>
2317                             Locklear 2011         <NA>          <NA>
2318                             Locklear 2011         <NA>          <NA>
2319                             Locklear 2011         <NA>          <NA>
2320                             Locklear 2011         <NA>          <NA>
2321                             Locklear 2011         <NA>          <NA>
2322                             Locklear 2011    butterfly          <NA>
2323                             Locklear 2011    butterfly          <NA>
2324                             Locklear 2011    butterfly          <NA>
2325                             Locklear 2011    butterfly          <NA>
2326                             Locklear 2011    butterfly          <NA>
2327                             Locklear 2011    butterfly          <NA>
2328                             Locklear 2011    butterfly          <NA>
2329                             Locklear 2011    butterfly          <NA>
2330                             Locklear 2011    butterfly          <NA>
2331                             Locklear 2011    butterfly          <NA>
2332                             Locklear 2011    butterfly          <NA>
2333                             Locklear 2011    butterfly          <NA>
2334                             Locklear 2011    butterfly          <NA>
2335                             Locklear 2011    butterfly          <NA>
2336                             Locklear 2011    butterfly          <NA>
2337                             Locklear 2011    butterfly          <NA>
2338                             Locklear 2011    butterfly          <NA>
2339                             Locklear 2011    butterfly          <NA>
2340                             Locklear 2011    butterfly          <NA>
2341                             Locklear 2011         <NA>          <NA>
2342                             Locklear 2011         <NA>          <NA>
2343                             Locklear 2011         <NA>          <NA>
2344                             Locklear 2011    butterfly          <NA>
2345                             Locklear 2011    butterfly          <NA>
2346                             Locklear 2011    butterfly          <NA>
2347                             Locklear 2011    butterfly          <NA>
2348                             Locklear 2011    butterfly          <NA>
2349                             Locklear 2011    butterfly          <NA>
2350                             Locklear 2011    butterfly          <NA>
2351                             Locklear 2011    butterfly          <NA>
2352                             Locklear 2011    butterfly          <NA>
2353                             Locklear 2011    butterfly          <NA>
2354                             Locklear 2011         <NA>          <NA>
2355                             Locklear 2011         <NA>          <NA>
2356                             Locklear 2011         <NA>          <NA>
2357                             Locklear 2011         <NA>          <NA>
2358                             Locklear 2011         <NA>          <NA>
2359                             Locklear 2011         <NA>          <NA>
2360                             Locklear 2011         <NA>          <NA>
2361                             Locklear 2011         <NA>          <NA>
2362                             Locklear 2011         <NA>          <NA>
2363                             Locklear 2011         <NA>          <NA>
2364                             Locklear 2011         <NA>          <NA>
2365                             Locklear 2011         <NA>          <NA>
2366                             Locklear 2011         <NA>          <NA>
2367                             Locklear 2011         <NA>          <NA>
2368                             Locklear 2011         <NA>          <NA>
2369                             Locklear 2011         <NA>          <NA>
2370                             Locklear 2011         <NA>          <NA>
2371                             Locklear 2011         <NA>          <NA>
2372                             Locklear 2011         <NA>          <NA>
2373                             Locklear 2011         <NA>          <NA>
2374                             Locklear 2011         <NA>          <NA>
2375                             Locklear 2011         <NA>          <NA>
2376                             Locklear 2011         <NA>          <NA>
2377                             Locklear 2011         <NA>          <NA>
2378                             Locklear 2011         <NA>          <NA>
2379                             Locklear 2011         <NA>          <NA>
2380                             Locklear 2011    butterfly          <NA>
2381                             Locklear 2011    butterfly          <NA>
2382                             Locklear 2011    butterfly          <NA>
2383                             Locklear 2011    butterfly          <NA>
2384                             Locklear 2011    butterfly          <NA>
2385                             Locklear 2011    butterfly          <NA>
2386                             Locklear 2011    butterfly          <NA>
2387                             Locklear 2011    butterfly          <NA>
2388                             Locklear 2011    butterfly          <NA>
2389                             Locklear 2011    butterfly          <NA>
2390                             Locklear 2011     hawkmoth          <NA>
2391                             Locklear 2011     hawkmoth          <NA>
2392                             Locklear 2011     hawkmoth          <NA>
2393                             Locklear 2011     hawkmoth          <NA>
2394                             Locklear 2011     hawkmoth          <NA>
2395                             Locklear 2011     hawkmoth          <NA>
2396                             Locklear 2011     hawkmoth          <NA>
2397                             Locklear 2011    butterfly          <NA>
2398                             Locklear 2011    butterfly          <NA>
2399                             Locklear 2011    butterfly          <NA>
2400                             Locklear 2011    butterfly          <NA>
2401                             Locklear 2011    butterfly          <NA>
2402                             Locklear 2011    butterfly          <NA>
2403                             Locklear 2011    butterfly          <NA>
2404                             Locklear 2011    butterfly          <NA>
2405                             Locklear 2011    butterfly          <NA>
2406                             Locklear 2011    butterfly          <NA>
2407                             Locklear 2011    butterfly          <NA>
2408                             Locklear 2011    butterfly          <NA>
2409                             Locklear 2011    butterfly          <NA>
2410                             Locklear 2011    butterfly          <NA>
2411                             Locklear 2011    butterfly       bee-fly
2412                             Locklear 2011    butterfly       bee-fly
2413                             Locklear 2011    butterfly       bee-fly
2414                             Locklear 2011    butterfly       bee-fly
2415                             Locklear 2011    butterfly       bee-fly
2416                             Locklear 2011    butterfly       bee-fly
2417                             Locklear 2011    butterfly       bee-fly
2418                             Locklear 2011    butterfly       bee-fly
2419                             Locklear 2011    butterfly       bee-fly
2420                             Locklear 2011    butterfly       bee-fly
2421                             Locklear 2011    butterfly       bee-fly
2422                             Locklear 2011    butterfly       bee-fly
2423                             Locklear 2011    butterfly       bee-fly
2424                             Locklear 2011    butterfly       bee-fly
2425                             Locklear 2011    butterfly       bee-fly
2426                             Locklear 2011    butterfly       bee-fly
2427                             Locklear 2011    butterfly       bee-fly
2428                             Locklear 2011    butterfly       bee-fly
2429                             Locklear 2011    butterfly       bee-fly
2430                             Locklear 2011    butterfly       bee-fly
2431                             Locklear 2011    butterfly       bee-fly
2432                             Locklear 2011         <NA>          <NA>
2433                             Locklear 2011         <NA>          <NA>
2434                             Locklear 2011         <NA>          <NA>
2435                             Locklear 2011         <NA>          <NA>
2436                             Locklear 2011         <NA>          <NA>
2437                             Locklear 2011         <NA>          <NA>
2438                             Locklear 2011         <NA>          <NA>
2439                             Locklear 2011         <NA>          <NA>
2440                             Locklear 2011         <NA>          <NA>
2441                             Locklear 2011         <NA>          <NA>
2442                             Locklear 2011         <NA>          <NA>
2443                             Locklear 2011         <NA>          <NA>
2444                             Locklear 2011         <NA>          <NA>
2445                             Locklear 2011         <NA>          <NA>
2446                             Locklear 2011         <NA>          <NA>
2447                             Locklear 2011         <NA>          <NA>
2448                             Locklear 2011         <NA>          <NA>
2449                             Locklear 2011         <NA>          <NA>
2450                             Locklear 2011         <NA>          <NA>
2451                             Locklear 2011         <NA>          <NA>
2452                             Locklear 2011         <NA>          <NA>
2453                             Locklear 2011         <NA>          <NA>
2454                             Locklear 2011         <NA>          <NA>
2455                             Locklear 2011         <NA>          <NA>
2456                             Locklear 2011         <NA>          <NA>
2457                             Locklear 2011         <NA>          <NA>
2458                             Locklear 2011         <NA>          <NA>
2459                             Locklear 2011         <NA>          <NA>
2460                             Locklear 2011         <NA>          <NA>
2461                             Locklear 2011         <NA>          <NA>
2462                                 CalPhotos         <NA>          <NA>
2463                                 CalPhotos         <NA>          <NA>
2464                                 CalPhotos         <NA>          <NA>
2465                                 CalPhotos         <NA>          <NA>
2466                                 CalPhotos         <NA>          <NA>
2467                                 CalPhotos         <NA>          <NA>
2468                                 CalPhotos         <NA>          <NA>
2469                                 CalPhotos         <NA>          <NA>
2470                                 CalPhotos         <NA>          <NA>
2471                                 CalPhotos         <NA>          <NA>
2472                         Grant  Grant 1965          bee         flies
2473                         Grant  Grant 1965          bee         flies
2474                         Grant  Grant 1965          bee         flies
2475                         Grant  Grant 1965          bee         flies
2476                         Grant  Grant 1965          bee          <NA>
2477                         Grant  Grant 1965          bee          <NA>
2478                         Grant  Grant 1965          bee          <NA>
2479                             Jepson Online        flies           bee
2480                             Jepson Online        flies           bee
2481                             Jepson Online        flies           bee
2482                             Jepson Online        flies           bee
2483                             Jepson Online        flies           bee
2484                             Jepson Online        flies           bee
2485                             Jepson Online        flies           bee
2486                             Jepson Online        flies           bee
2487                             Jepson Online        flies           bee
2488                             Jepson Online        flies           bee
2489                             Jepson Online         <NA>          <NA>
2490                             Jepson Online         <NA>          <NA>
2491                             Jepson Online         <NA>          <NA>
2492                             Jepson Online         <NA>          <NA>
2493                   Flora Croatica Database         <NA>          <NA>
2494                   Flora Croatica Database         <NA>          <NA>
2495                   Flora Croatica Database         <NA>          <NA>
2496                   Flora Croatica Database         <NA>          <NA>
2497                   Flora Croatica Database         <NA>          <NA>
2498                   Flora Croatica Database         <NA>          <NA>
2499                   Flora Croatica Database         <NA>          <NA>
2500                   Flora Croatica Database         <NA>          <NA>
2501                   Flora Croatica Database         <NA>          <NA>
2502                             Jepson Online         <NA>          <NA>
2503                             Jepson Online         <NA>          <NA>
2504                             Jepson Online         <NA>          <NA>
2505                             Jepson Online         <NA>          <NA>
2506                         Grant  Grant 1965          bee  hummingbirds
2507                         Grant  Grant 1965          bee  hummingbirds
2508                         Grant  Grant 1965          bee  hummingbirds
2509                         Grant  Grant 1965          bee  hummingbirds
2510                         Grant  Grant 1965          bee  hummingbirds
2511                         Grant  Grant 1965          bee  hummingbirds
2512                         Grant  Grant 1965          bee  hummingbirds
2513                         Grant  Grant 1965          bee  hummingbirds
2514                         Grant  Grant 1965          bee  hummingbirds
2515                         Grant  Grant 1965          bee  hummingbirds
2516                         Grant  Grant 1965          bee  hummingbirds
2517                         Grant  Grant 1965          bee  hummingbirds
2518                         Grant  Grant 1965          bee  hummingbirds
2519                         Grant  Grant 1965          bee  hummingbirds
2520                         Grant  Grant 1965          bee  hummingbirds
2521                         Grant  Grant 1965          bee  hummingbirds
2522          Univ. of Wyoming Digital Archive        flies           bee
2523          Univ. of Wyoming Digital Archive        flies           bee
2524          Univ. of Wyoming Digital Archive        flies           bee
2525          Univ. of Wyoming Digital Archive        flies           bee
2526          Univ. of Wyoming Digital Archive        flies           bee
2527          Univ. of Wyoming Digital Archive        flies           bee
2528          Univ. of Wyoming Digital Archive        flies           bee
2529          Univ. of Wyoming Digital Archive        flies           bee
2530          Univ. of Wyoming Digital Archive        flies           bee
2531          Univ. of Wyoming Digital Archive        flies           bee
2532                         Grant  Grant 1965        flies          <NA>
2533                         Grant  Grant 1965        flies          <NA>
2534                         Grant  Grant 1965        flies          <NA>
2535                         Grant  Grant 1965        flies          <NA>
2536                         Grant  Grant 1965        flies          <NA>
2537                         Grant  Grant 1965        flies          <NA>
2538                         Grant  Grant 1965        flies          <NA>
2539                         Grant  Grant 1965        flies          <NA>
2540                         Grant  Grant 1965        flies          <NA>
2541                         Grant  Grant 1965        flies          <NA>
2542                         Grant  Grant 1965        flies          <NA>
2543                         Grant  Grant 1965        flies          <NA>
2544                         Grant  Grant 1965        flies          <NA>
2545                         Grant  Grant 1965        flies          <NA>
2546                         Grant  Grant 1965          bee          <NA>
2547                         Grant  Grant 1965          bee          <NA>
2548                         Grant  Grant 1965          bee          <NA>
2549                         Grant  Grant 1965          bee          <NA>
2550                         Grant  Grant 1965          bee          <NA>
2551                         Grant  Grant 1965          bee          <NA>
2552                         Grant  Grant 1965          bee          <NA>
2553                         Grant  Grant 1965          bee          <NA>
2554                         Grant  Grant 1965          bee          <NA>
2555                         Grant  Grant 1965          bee          <NA>
2556                         Grant  Grant 1965          bee          <NA>
2557                         Grant  Grant 1965          bee          <NA>
2558                         Grant  Grant 1965          bee          <NA>
2559                         Grant  Grant 1965          bee          <NA>
2560                         Grant  Grant 1965          bee          <NA>
2561                         Grant  Grant 1965          bee          <NA>
2562                         Grant  Grant 1965          bee          <NA>
2563                         Grant  Grant 1965          bee          <NA>
2564                         Grant  Grant 1965          bee          <NA>
2565                         Grant  Grant 1965          bee          <NA>
2566                         Grant  Grant 1965          bee          <NA>
2567 France National Museum of Natural History         <NA>          <NA>
2568 France National Museum of Natural History         <NA>          <NA>
2569 France National Museum of Natural History         <NA>          <NA>
2570 France National Museum of Natural History         <NA>          <NA>
2571 France National Museum of Natural History         <NA>          <NA>
2572 France National Museum of Natural History         <NA>          <NA>
2573 France National Museum of Natural History         <NA>          <NA>
2574                         Grant  Grant 1965         <NA>          <NA>
2575                         Grant  Grant 1965         <NA>          <NA>
2576                         Grant  Grant 1965         <NA>          <NA>
2577                         Grant  Grant 1965         <NA>          <NA>
2578                         Grant  Grant 1965         <NA>          <NA>
2579                         Grant  Grant 1965         <NA>          <NA>
2580                         Grant  Grant 1965         <NA>          <NA>
2581                         Grant  Grant 1965         <NA>          <NA>
2582                         Grant  Grant 1965         <NA>          <NA>
2583                         Grant  Grant 1965         <NA>          <NA>
2584                         Grant  Grant 1965         <NA>          <NA>
2585                         Grant  Grant 1965         <NA>          <NA>
2586                         Grant  Grant 1965         <NA>          <NA>
2587                         Grant  Grant 1965         <NA>          <NA>
2588                         Grant  Grant 1965         <NA>          <NA>
2589                         Grant  Grant 1965         <NA>          <NA>
2590                         Grant  Grant 1965         <NA>          <NA>
2591                         Grant  Grant 1965         <NA>          <NA>
2592                             Jepson Online         <NA>          <NA>
2593                             Jepson Online         <NA>          <NA>
2594                             Jepson Online         <NA>          <NA>
2595                             Jepson Online         <NA>          <NA>
2596                             Jepson Online         <NA>          <NA>
2597                             Jepson Online         <NA>          <NA>
2598                             Jepson Online         <NA>          <NA>
2599                             Jepson Online         <NA>          <NA>
2600                             Jepson Online         <NA>          <NA>
2601                             Jepson Online         <NA>          <NA>
2602                             Jepson Online         <NA>          <NA>
2603                             Jepson Online         <NA>          <NA>
2604                             Jepson Online         <NA>          <NA>
2605                             Jepson Online         <NA>          <NA>
2606                             Jepson Online         <NA>          <NA>
2607                             Jepson Online         <NA>          <NA>
2608                         Grant  Grant 1965  hummingbird          <NA>
2609                         Grant  Grant 1965  hummingbird          <NA>
2610                         Grant  Grant 1965  hummingbird          <NA>
2611                Flora of Pacific Northwest         <NA>          <NA>
2612                Flora of Pacific Northwest         <NA>          <NA>
2613                Flora of Pacific Northwest         <NA>          <NA>
2614                Flora of Pacific Northwest         <NA>          <NA>
2615                Flora of Pacific Northwest         <NA>          <NA>
2616                Flora of Pacific Northwest         <NA>          <NA>
2617                Flora of Pacific Northwest         <NA>          <NA>
2618                Flora of Pacific Northwest         <NA>          <NA>
2619                Flora of Pacific Northwest         <NA>          <NA>
2620                         Grant  Grant 1965          bee         flies
2621                         Grant  Grant 1965          bee         flies
2622                         Grant  Grant 1965          bee         flies
2623                         Grant  Grant 1965          bee         flies
2624                         Grant  Grant 1965          bee         flies
2625                         Grant  Grant 1965          bee         flies
2626                         Grant  Grant 1965          bee         flies
2627                         Grant  Grant 1965          bee         flies
2628                         Grant  Grant 1965          bee         flies
2629                         Grant  Grant 1965          bee         flies
2630                         Grant  Grant 1965          bee         flies
2631                         Grant  Grant 1965          bee         flies
2632                         Grant  Grant 1965          bee         flies
2633                         Grant  Grant 1965          bee         flies
2634                         Grant  Grant 1965          bee         flies
2635                         Grant  Grant 1965          bee         flies
2636                         Grant  Grant 1965          bee         flies
2637                         Grant  Grant 1965          bee         flies
2638                         Grant  Grant 1965          bee         flies
2639                         Grant  Grant 1965          bee         flies
2640                         Grant  Grant 1965          bee         flies
2641                         Grant  Grant 1965          bee         flies
2642                         Grant  Grant 1965          bee         flies
2643                         Grant  Grant 1965          bee         flies
2644                         Grant  Grant 1965          bee         flies
2645                         Grant  Grant 1965          bee         flies
2646                         Grant  Grant 1965          bee         flies
2647                         Grant  Grant 1965          bee         flies
2648                 Flora of British Columbia          bee         flies
2649                 Flora of British Columbia          bee         flies
2650                 Flora of British Columbia          bee         flies
2651                 Flora of British Columbia          bee         flies
2652                 Flora of British Columbia          bee         flies
2653                 Flora of British Columbia          bee         flies
2654                 Flora of British Columbia          bee         flies
2655                 Flora of British Columbia          bee         flies
2656                 Flora of British Columbia          bee         flies
2657                 Flora of British Columbia          bee         flies
2658                 Flora of British Columbia          bee         flies
2659                 Flora of British Columbia          bee         flies
2660                             Jepson Online         <NA>          <NA>
2661                             Jepson Online         <NA>          <NA>
2662                             Jepson Online         <NA>          <NA>
2663                             Jepson Online         <NA>          <NA>
2664                             Jepson Online         <NA>          <NA>
2665                             Jepson Online         <NA>          <NA>
2666                             Jepson Online         <NA>          <NA>
2667                             Jepson Online         <NA>          <NA>
2668                             Jepson Online         <NA>          <NA>
2669                             Jepson Online         <NA>          <NA>
2670                             Jepson Online         <NA>          <NA>
2671                             Jepson Online         <NA>          <NA>
2672                             Jepson Online         <NA>          <NA>
2673                             Jepson Online         <NA>          <NA>
2674                             Jepson Online         <NA>          <NA>
2675                             Jepson Online         <NA>          <NA>
2676                             Jepson Online          bee          <NA>
2677                             Jepson Online          bee          <NA>
2678                             Jepson Online          bee          <NA>
2679                             Jepson Online          bee          <NA>
2680                             Jepson Online          bee          <NA>
2681                             Jepson Online          bee          <NA>
2682                             Jepson Online          bee          <NA>
2683                             Jepson Online          bee          <NA>
2684                             Jepson Online          bee          <NA>
2685                             Jepson Online          bee          <NA>
2686                             Jepson Online         <NA>          <NA>
2687                             Jepson Online         <NA>          <NA>
2688                             Jepson Online         <NA>          <NA>
2689                             Jepson Online         <NA>          <NA>
     pollinator_3               pollinator_source
1            <NA>                 Hsu,  Hall 2003
2            <NA>                 Hsu,  Hall 2003
3            <NA>                 Hsu,  Hall 2003
4            <NA>                 Hsu,  Hall 2003
5            <NA>                 Hsu,  Hall 2003
6            <NA>              Grant,  Grant 1965
7            <NA>              Grant,  Grant 1965
8            <NA>              Grant,  Grant 1965
9            <NA>              Grant,  Grant 1965
10           <NA>                            <NA>
11           <NA>                            <NA>
12           <NA>                            <NA>
13           <NA>                            <NA>
14           <NA>                            <NA>
15           <NA>                            <NA>
16           <NA>                            <NA>
17           <NA>                            <NA>
18           <NA>                            <NA>
19           <NA>                            <NA>
20           <NA>                            <NA>
21           <NA>                            <NA>
22           <NA>                            <NA>
23           <NA>                            <NA>
24           <NA>                            <NA>
25           <NA>                            <NA>
26           <NA>                            <NA>
27           <NA>                            <NA>
28           <NA>                            <NA>
29           <NA>                            <NA>
30           <NA>                            <NA>
31           <NA>                            <NA>
32           <NA>                            <NA>
33           <NA>                            <NA>
34           <NA>                            <NA>
35           <NA>                            <NA>
36           <NA>                            <NA>
37           <NA>                            <NA>
38           <NA>                            <NA>
39           <NA>                            <NA>
40           <NA>                            <NA>
41           <NA>                            <NA>
42           <NA>                            <NA>
43           <NA>              Grant,  Grant 1965
44           <NA>              Grant,  Grant 1965
45           <NA>              Grant,  Grant 1965
46           <NA>              Grant,  Grant 1965
47           <NA>              Grant,  Grant 1965
48           <NA>              Grant,  Grant 1965
49           <NA>              Grant,  Grant 1965
50           <NA>                            <NA>
51           <NA>                            <NA>
52           <NA>                            <NA>
53           <NA>                            <NA>
54           <NA>                            <NA>
55           <NA>                            <NA>
56           <NA>                            <NA>
57           <NA>                            <NA>
58           <NA>                            <NA>
59           <NA>                            <NA>
60           <NA>              Grant,  Grant 1965
61           <NA>              Grant,  Grant 1965
62           <NA>              Grant,  Grant 1965
63           <NA>              Grant,  Grant 1965
64           <NA>              Grant,  Grant 1965
65           <NA>              Grant,  Grant 1965
66           <NA>              Grant,  Grant 1965
67           <NA>              Grant,  Grant 1965
68           <NA>              Grant,  Grant 1965
69           <NA>              Grant,  Grant 1965
70           <NA>                            <NA>
71           <NA>                            <NA>
72           <NA>                            <NA>
73           <NA>                            <NA>
74           <NA>                            <NA>
75           <NA>                            <NA>
76           <NA>                            <NA>
77           <NA>                            <NA>
78           <NA>              Grant,  Grant 1965
79           <NA>              Grant,  Grant 1965
80           <NA>              Grant,  Grant 1965
81           <NA>              Grant,  Grant 1965
82           <NA>                            <NA>
83           <NA>                            <NA>
84           <NA>                            <NA>
85           <NA>                            <NA>
86           <NA>                            <NA>
87           <NA>                            <NA>
88           <NA>                            <NA>
89           <NA>                            <NA>
90           <NA>                            <NA>
91           <NA>                            <NA>
92           <NA>                            <NA>
93           <NA>                            <NA>
94           <NA>                            <NA>
95           <NA>                            <NA>
96           <NA>                            <NA>
97           <NA>                            <NA>
98           <NA>                            <NA>
99           <NA>                            <NA>
100          <NA>                            <NA>
101          <NA>                            <NA>
102          <NA>                            <NA>
103          <NA>                            <NA>
104          <NA>                            <NA>
105          <NA>                            <NA>
106          <NA>                            <NA>
107          <NA>                            <NA>
108          <NA>              Grant,  Grant 1965
109          <NA>              Grant,  Grant 1965
110          <NA>              Grant,  Grant 1965
111          <NA>              Grant,  Grant 1965
112          <NA>              Grant,  Grant 1965
113          <NA>              Grant,  Grant 1965
114          <NA>              Grant,  Grant 1965
115          <NA>              Grant,  Grant 1965
116          <NA>              Grant,  Grant 1965
117          <NA>              Grant,  Grant 1965
118          <NA>                            <NA>
119          <NA>                            <NA>
120          <NA>                            <NA>
121          <NA>                            <NA>
122          <NA>                            <NA>
123          <NA>                            <NA>
124          <NA>                            <NA>
125          <NA>                            <NA>
126          <NA>                            <NA>
127          <NA>                            <NA>
128          <NA>                            <NA>
129          <NA>                            <NA>
130          <NA>                            <NA>
131          <NA>                            <NA>
132          <NA>                            <NA>
133          <NA>                            <NA>
134          <NA>                            <NA>
135          <NA>                            <NA>
136          <NA>                            <NA>
137          <NA>                            <NA>
138          <NA>                            <NA>
139          <NA>                            <NA>
140          <NA>                            <NA>
141          <NA>                            <NA>
142          <NA>              Grant,  Grant 1965
143          <NA>              Grant,  Grant 1965
144          <NA>              Grant,  Grant 1965
145          <NA>              Grant,  Grant 1965
146          <NA>              Grant,  Grant 1965
147          <NA>              Grant,  Grant 1965
148          <NA>              Grant,  Grant 1965
149          <NA>              Grant,  Grant 1965
150          <NA>              Grant,  Grant 1965
151          <NA>              Grant,  Grant 1965
152          <NA>              Grant,  Grant 1965
153          <NA>              Grant,  Grant 1965
154          <NA>              Grant,  Grant 1965
155          <NA>              Grant,  Grant 1965
156          <NA>              Grant,  Grant 1965
157          <NA>              Grant,  Grant 1965
158          <NA>              Grant,  Grant 1965
159          <NA>              Grant,  Grant 1965
160          <NA>              Grant,  Grant 1965
161          <NA>                            <NA>
162          <NA>                            <NA>
163          <NA>                            <NA>
164          <NA>                            <NA>
165          <NA>                            <NA>
166          <NA>                            <NA>
167          <NA>                            <NA>
168          <NA>                            <NA>
169          <NA>                            <NA>
170          <NA>                            <NA>
171          <NA>                            <NA>
172          <NA>                            <NA>
173          <NA>                            <NA>
174          <NA>              Grant,  Grant 1965
175          <NA>              Grant,  Grant 1965
176          <NA>              Grant,  Grant 1965
177          <NA>              Grant,  Grant 1965
178          <NA>              Grant,  Grant 1965
179          <NA>              Grant,  Grant 1965
180          <NA>              Grant,  Grant 1965
181          <NA>              Grant,  Grant 1965
182          <NA>              Grant,  Grant 1965
183          <NA>              Grant,  Grant 1965
184          <NA>              Grant,  Grant 1965
185          <NA>              Grant,  Grant 1965
186          <NA>              Grant,  Grant 1965
187          <NA>              Grant,  Grant 1965
188          <NA>              Grant,  Grant 1965
189          <NA>              Grant,  Grant 1965
190          <NA>              Grant,  Grant 1965
191          <NA>              Grant,  Grant 1965
192          <NA>              Grant,  Grant 1965
193          <NA>              Grant,  Grant 1965
194          <NA>              Grant,  Grant 1965
195          <NA>              Grant,  Grant 1965
196          <NA>              Grant,  Grant 1965
197          <NA>              Grant,  Grant 1965
198          <NA>              Grant,  Grant 1965
199          <NA>              Grant,  Grant 1965
200          <NA>              Grant,  Grant 1965
201          <NA>              Grant,  Grant 1965
202          <NA>              Grant,  Grant 1965
203          <NA>              Grant,  Grant 1965
204          <NA>              Grant,  Grant 1965
205          <NA>              Grant,  Grant 1965
206          <NA>              Grant,  Grant 1965
207          <NA>              Grant,  Grant 1965
208          <NA>              Grant,  Grant 1965
209          <NA>              Grant,  Grant 1965
210          <NA>              Grant,  Grant 1965
211          <NA>              Grant,  Grant 1965
212          <NA>              Grant,  Grant 1965
213          <NA>              Grant,  Grant 1965
214          <NA>              Grant,  Grant 1965
215          <NA>              Grant,  Grant 1965
216          <NA>              Grant,  Grant 1965
217          <NA>              Grant,  Grant 1965
218          <NA>              Grant,  Grant 1965
219          <NA>              Grant,  Grant 1965
220          <NA>              Grant,  Grant 1965
221          <NA>              Grant,  Grant 1965
222          <NA>              Grant,  Grant 1965
223          <NA>              Grant,  Grant 1965
224          <NA>              Grant,  Grant 1965
225          <NA>              Grant,  Grant 1965
226          <NA>              Grant,  Grant 1965
227          <NA>              Grant,  Grant 1965
228          <NA>              Grant,  Grant 1965
229          <NA>              Grant,  Grant 1965
230          <NA>              Grant,  Grant 1965
231          <NA>              Grant,  Grant 1965
232          <NA>              Grant,  Grant 1965
233          <NA>              Grant,  Grant 1965
234          <NA>              Grant,  Grant 1965
235          <NA>              Grant,  Grant 1965
236          <NA>              Grant,  Grant 1965
237          <NA>              Grant,  Grant 1965
238          <NA>              Grant,  Grant 1965
239          <NA>              Grant,  Grant 1965
240          <NA>              Grant,  Grant 1965
241          <NA>              Grant,  Grant 1965
242          <NA>                            <NA>
243          <NA>                            <NA>
244          <NA>                            <NA>
245          <NA>                            <NA>
246          <NA>              Grant,  Grant 1965
247          <NA>              Grant,  Grant 1965
248          <NA>              Grant,  Grant 1965
249          <NA>              Grant,  Grant 1965
250          <NA>              Grant,  Grant 1965
251          <NA>              Grant,  Grant 1965
252          <NA>              Grant,  Grant 1965
253          <NA>              Grant,  Grant 1965
254          <NA>              Grant,  Grant 1965
255          <NA>              Grant,  Grant 1965
256          <NA>              Grant,  Grant 1965
257          <NA>              Grant,  Grant 1965
258          <NA>              Grant,  Grant 1965
259          <NA>              Grant,  Grant 1965
260          <NA>              Grant,  Grant 1965
261          <NA>              Grant,  Grant 1965
262          <NA>              Grant,  Grant 1965
263          <NA>                            <NA>
264          <NA>                            <NA>
265          <NA>                            <NA>
266          <NA>                            <NA>
267          <NA>                            <NA>
268          <NA>                            <NA>
269          <NA>                            <NA>
270          <NA>                            <NA>
271          <NA>                            <NA>
272          <NA>                            <NA>
273          <NA>                            <NA>
274          <NA>                            <NA>
275          <NA>                            <NA>
276          <NA>                            <NA>
277          <NA>                            <NA>
278          <NA>                            <NA>
279          <NA>                            <NA>
280          <NA>                            <NA>
281          <NA>                            <NA>
282          <NA>                            <NA>
283          <NA>                            <NA>
284          <NA>                            <NA>
285          <NA>                            <NA>
286          <NA>                            <NA>
287          <NA>                            <NA>
288          <NA>                            <NA>
289          <NA>                            <NA>
290          <NA>                            <NA>
291          <NA>              Grant,  Grant 1965
292          <NA>              Grant,  Grant 1965
293          <NA>              Grant,  Grant 1965
294          <NA>              Grant,  Grant 1965
295          <NA>              Grant,  Grant 1965
296          <NA>              Grant,  Grant 1965
297          <NA>              Grant,  Grant 1965
298          <NA>              Grant,  Grant 1965
299          <NA>              Grant,  Grant 1965
300          <NA>              Grant,  Grant 1965
301          <NA>              Grant,  Grant 1965
302          <NA>              Grant,  Grant 1965
303          <NA>              Grant,  Grant 1965
304          <NA>                            <NA>
305          <NA>                            <NA>
306          <NA>                            <NA>
307          <NA>                            <NA>
308          <NA>                   Prather 1999b
309          <NA>                   Prather 1999b
310          <NA>                            <NA>
311          <NA>                            <NA>
312          <NA>                            <NA>
313          <NA>                            <NA>
314          <NA>                            <NA>
315          <NA>                            <NA>
316          <NA>                            <NA>
317          <NA>                            <NA>
318          <NA>                            <NA>
319          <NA>                   Prather 1999b
320          <NA>                   Prather 1999b
321          <NA>                            <NA>
322          <NA>                            <NA>
323          <NA>                            <NA>
324          <NA>                            <NA>
325          <NA>                            <NA>
326          <NA>                            <NA>
327          <NA>                            <NA>
328          <NA>                            <NA>
329          <NA>                   Prather 1999b
330          <NA>                   Prather 1999b
331          <NA>                   Prather 1999b
332          <NA>                   Prather 1999b
333          <NA>                   Prather 1999b
334          <NA>                   Prather 1999b
335          <NA>                   Prather 1999b
336          <NA>                   Prather 1999b
337          <NA>                   Prather 1999b
338          <NA>                   Prather 1999b
339          <NA>                   Prather 1999b
340          <NA>                   Prather 1999b
341          <NA>                   Prather 1999b
342          <NA>                   Prather 1999b
343          <NA>                   Prather 1999b
344          <NA>                   Prather 1999b
345          <NA>                   Prather 1999b
346          <NA>                   Prather 1999b
347          <NA>                            <NA>
348          <NA>                            <NA>
349          <NA>                            <NA>
350          <NA>                            <NA>
351          <NA>                            <NA>
352          <NA>                            <NA>
353          <NA>                            <NA>
354          <NA>                   Prather 1999b
355          <NA>                   Prather 1999b
356          <NA>                   Prather 1999b
357          <NA>                            <NA>
358          <NA>                   Prather 1999b
359          <NA>                   Prather 1999b
360          <NA>                   Prather 1999b
361          <NA>                   Prather 1999b
362          <NA>                   Prather 1999b
363          <NA>                   Prather 1999b
364          <NA>                   Prather 1999b
365          <NA>                   Prather 1999b
366          <NA>                   Prather 1999b
367          <NA>                   Prather 1999b
368          <NA>                            <NA>
369          <NA>                            <NA>
370          <NA>                            <NA>
371          <NA>                   Prather 1999b
372          <NA>                   Prather 1999b
373          <NA>                   Prather 1999b
374          <NA>                   Prather 1999b
375          <NA>                            <NA>
376          <NA>                            <NA>
377          <NA>                            <NA>
378          <NA>                            <NA>
379          <NA>                            <NA>
380          <NA>                            <NA>
381          <NA>                            <NA>
382          <NA>                            <NA>
383          <NA>                            <NA>
384          <NA>                            <NA>
385          <NA>                            <NA>
386          <NA>                            <NA>
387          <NA>                            <NA>
388          <NA>                            <NA>
389          <NA>                            <NA>
390          <NA>                            <NA>
391          <NA>                            <NA>
392          <NA>                            <NA>
393          <NA>                            <NA>
394          <NA>                            <NA>
395          <NA>                            <NA>
396          <NA>                            <NA>
397          <NA>                            <NA>
398          <NA>                            <NA>
399          <NA>                            <NA>
400          <NA>              Grant,  Grant 1965
401          <NA>              Grant,  Grant 1965
402          <NA>              Grant,  Grant 1965
403          <NA>              Grant,  Grant 1965
404          <NA>              Grant,  Grant 1965
405          <NA>              Grant,  Grant 1965
406          <NA>              Grant,  Grant 1965
407          <NA>              Grant,  Grant 1965
408          <NA>              Grant,  Grant 1965
409          <NA>              Grant,  Grant 1965
410          <NA>              Grant,  Grant 1965
411          <NA>              Grant,  Grant 1965
412          <NA>              Grant,  Grant 1965
413          <NA>              Grant,  Grant 1965
414          <NA>              Grant,  Grant 1965
415          <NA>              Grant,  Grant 1965
416          <NA>              Grant,  Grant 1965
417          <NA>              Grant,  Grant 1965
418          <NA>              Grant,  Grant 1965
419          <NA>              Grant,  Grant 1965
420          <NA>              Grant,  Grant 1965
421          <NA>              Grant,  Grant 1965
422          <NA>              Grant,  Grant 1965
423          <NA>              Grant,  Grant 1965
424          <NA>              Grant,  Grant 1965
425          <NA>              Grant,  Grant 1965
426          <NA>              Grant,  Grant 1965
427          <NA>              Grant,  Grant 1965
428          <NA>              Grant,  Grant 1965
429          <NA>              Grant,  Grant 1965
430          <NA>              Grant,  Grant 1965
431          <NA>              Grant,  Grant 1965
432          <NA>              Grant,  Grant 1965
433          <NA>              Grant,  Grant 1965
434          <NA>              Grant,  Grant 1965
435          <NA>              Grant,  Grant 1965
436          <NA>              Grant,  Grant 1965
437          <NA>              Grant,  Grant 1965
438          <NA>              Grant,  Grant 1965
439          <NA>              Grant,  Grant 1965
440          <NA>              Grant,  Grant 1965
441          <NA>              Grant,  Grant 1965
442          <NA>              Grant,  Grant 1965
443          <NA>              Grant,  Grant 1965
444          <NA>              Grant,  Grant 1965
445          <NA>              Grant,  Grant 1965
446          <NA>              Grant,  Grant 1965
447          <NA>              Grant,  Grant 1965
448          <NA>              Grant,  Grant 1965
449          <NA>              Grant,  Grant 1965
450          <NA>              Grant,  Grant 1965
451          <NA>              Grant,  Grant 1965
452          <NA>              Grant,  Grant 1965
453          <NA>              Grant,  Grant 1965
454          <NA>              Grant,  Grant 1965
455          <NA>              Grant,  Grant 1965
456          <NA>              Grant,  Grant 1965
457          <NA>              Grant,  Grant 1965
458          <NA>              Grant,  Grant 1965
459          <NA>              Grant,  Grant 1965
460          <NA>              Grant,  Grant 1965
461          <NA>              Grant,  Grant 1965
462          <NA>              Grant,  Grant 1965
463          <NA>              Grant,  Grant 1965
464          <NA>              Grant,  Grant 1965
465          <NA>  Joyal 1986; Grant,  Grant 1965
466          <NA>  Joyal 1986; Grant,  Grant 1965
467          <NA>  Joyal 1986; Grant,  Grant 1965
468          <NA>  Joyal 1986; Grant,  Grant 1965
469          <NA>  Joyal 1986; Grant,  Grant 1965
470          <NA>  Joyal 1986; Grant,  Grant 1965
471          <NA>  Joyal 1986; Grant,  Grant 1965
472          <NA>  Joyal 1986; Grant,  Grant 1965
473          <NA>  Joyal 1986; Grant,  Grant 1965
474          <NA>  Joyal 1986; Grant,  Grant 1965
475          <NA>  Joyal 1986; Grant,  Grant 1965
476          <NA>  Joyal 1986; Grant,  Grant 1965
477          <NA>  Joyal 1986; Grant,  Grant 1965
478          <NA>  Joyal 1986; Grant,  Grant 1965
479          <NA>  Joyal 1986; Grant,  Grant 1965
480          <NA>  Joyal 1986; Grant,  Grant 1965
481          <NA>  Joyal 1986; Grant,  Grant 1965
482          <NA>  Joyal 1986; Grant,  Grant 1965
483          <NA>  Joyal 1986; Grant,  Grant 1965
484          <NA>  Joyal 1986; Grant,  Grant 1965
485          <NA>  Joyal 1986; Grant,  Grant 1965
486          <NA>  Joyal 1986; Grant,  Grant 1965
487          <NA>  Joyal 1986; Grant,  Grant 1965
488          <NA>  Joyal 1986; Grant,  Grant 1965
489          <NA>  Joyal 1986; Grant,  Grant 1965
490          <NA>  Joyal 1986; Grant,  Grant 1965
491          <NA>  Joyal 1986; Grant,  Grant 1965
492          <NA>  Joyal 1986; Grant,  Grant 1965
493          <NA>  Joyal 1986; Grant,  Grant 1965
494          <NA>  Joyal 1986; Grant,  Grant 1965
495          <NA>  Joyal 1986; Grant,  Grant 1965
496          <NA>  Joyal 1986; Grant,  Grant 1965
497          <NA>  Joyal 1986; Grant,  Grant 1965
498          <NA>  Joyal 1986; Grant,  Grant 1965
499          <NA>  Joyal 1986; Grant,  Grant 1965
500          <NA>  Joyal 1986; Grant,  Grant 1965
501          <NA>  Joyal 1986; Grant,  Grant 1965
502          <NA>  Joyal 1986; Grant,  Grant 1965
503          <NA>  Joyal 1986; Grant,  Grant 1965
504          <NA>  Joyal 1986; Grant,  Grant 1965
505          <NA>              Grant,  Grant 1965
506          <NA>              Grant,  Grant 1965
507          <NA>              Grant,  Grant 1965
508          <NA>              Grant,  Grant 1965
509          <NA>              Grant,  Grant 1965
510          <NA>              Grant,  Grant 1965
511          <NA>              Grant,  Grant 1965
512          <NA>              Grant,  Grant 1965
513          <NA>              Grant,  Grant 1965
514          <NA>              Grant,  Grant 1965
515          <NA>              Grant,  Grant 1965
516          <NA>              Grant,  Grant 1965
517          <NA>              Grant,  Grant 1965
518          <NA>              Grant,  Grant 1965
519          <NA>              Grant,  Grant 1965
520          <NA>              Grant,  Grant 1965
521          <NA>              Grant,  Grant 1965
522          <NA>                            <NA>
523          <NA>                            <NA>
524          <NA>                            <NA>
525          <NA>                            <NA>
526          <NA>                            <NA>
527          <NA>                            <NA>
528          <NA>                            <NA>
529          <NA>                            <NA>
530          <NA>                            <NA>
531          <NA>                            <NA>
532          <NA>                            <NA>
533          <NA>                            <NA>
534          <NA>                            <NA>
535          <NA>                      Joyal 1986
536          <NA>                      Joyal 1986
537          <NA>                      Joyal 1986
538          <NA>                      Joyal 1986
539          <NA>                      Joyal 1986
540          <NA>                      Joyal 1986
541          <NA>                      Joyal 1986
542          <NA>                      Joyal 1986
543          <NA>                      Joyal 1986
544          <NA>                      Joyal 1986
545          <NA>                            <NA>
546          <NA>                            <NA>
547          <NA>                            <NA>
548          <NA>                            <NA>
549          <NA>                            <NA>
550          <NA>                            <NA>
551          <NA>                            <NA>
552          <NA>                            <NA>
553          <NA>                            <NA>
554           bee Rancho Santa Ana Botanic Garden
555           bee Rancho Santa Ana Botanic Garden
556           bee Rancho Santa Ana Botanic Garden
557          <NA>                            <NA>
558          <NA>                            <NA>
559          <NA>                            <NA>
560          <NA>                            <NA>
561          <NA>                            <NA>
562          <NA>                            <NA>
563          <NA>                            <NA>
564          <NA>                            <NA>
565          <NA>                            <NA>
566          <NA>                            <NA>
567          <NA>                            <NA>
568          <NA>                            <NA>
569          <NA>                            <NA>
570          <NA>                            <NA>
571          <NA>                            <NA>
572          <NA>              Grant,  Grant 1965
573          <NA>              Grant,  Grant 1965
574          <NA>              Grant,  Grant 1965
575          <NA>              Grant,  Grant 1965
576          <NA>              Grant,  Grant 1965
577          <NA>              Grant,  Grant 1965
578          <NA>              Grant,  Grant 1965
579          <NA>              Grant,  Grant 1965
580          <NA>              Grant,  Grant 1965
581          <NA>              Grant,  Grant 1965
582          <NA>              Grant,  Grant 1965
583          <NA>              Grant,  Grant 1965
584          <NA>              Grant,  Grant 1965
585          <NA>              Grant,  Grant 1965
586          <NA>              Grant,  Grant 1965
587          <NA>              Grant,  Grant 1965
588          <NA>              Grant,  Grant 1965
589          <NA>              Grant,  Grant 1965
590          <NA>              Grant,  Grant 1965
591          <NA>              Grant,  Grant 1965
592          <NA>              Grant,  Grant 1965
593          <NA>              Grant,  Grant 1965
594          <NA>                            <NA>
595          <NA>                            <NA>
596          <NA>                            <NA>
597          <NA>                            <NA>
598          <NA>                            <NA>
599          <NA>                            <NA>
600          <NA>                            <NA>
601          <NA>                            <NA>
602          <NA>                            <NA>
603          <NA>                            <NA>
604          <NA>              Grant,  Grant 1965
605          <NA>              Grant,  Grant 1965
606          <NA>              Grant,  Grant 1965
607          <NA>              Grant,  Grant 1965
608          <NA>              Grant,  Grant 1965
609          <NA>              Grant,  Grant 1965
610          <NA>              Grant,  Grant 1965
611          <NA>              Grant,  Grant 1965
612          <NA>              Grant,  Grant 1965
613          <NA>              Grant,  Grant 1965
614          <NA>              Grant,  Grant 1965
615          <NA>              Grant,  Grant 1965
616          <NA>              Grant,  Grant 1965
617          <NA>              Grant,  Grant 1965
618          <NA>              Grant,  Grant 1965
619          <NA>              Grant,  Grant 1965
620          <NA>              Grant,  Grant 1965
621          <NA>              Grant,  Grant 1965
622          <NA>              Grant,  Grant 1965
623          <NA>              Grant,  Grant 1965
624          <NA>              Grant,  Grant 1965
625          <NA>              Grant,  Grant 1965
626          <NA>                            <NA>
627          <NA>                            <NA>
628          <NA>                            <NA>
629          <NA>                            <NA>
630          <NA>                            <NA>
631          <NA>                            <NA>
632          <NA>                            <NA>
633          <NA>                            <NA>
634          <NA>                            <NA>
635          <NA>                            <NA>
636          <NA>              Grant,  Grant 1965
637          <NA>              Grant,  Grant 1965
638          <NA>              Grant,  Grant 1965
639          <NA>              Grant,  Grant 1965
640          <NA>              Grant,  Grant 1965
641          <NA>              Grant,  Grant 1965
642          <NA>              Grant,  Grant 1965
643          <NA>              Grant,  Grant 1965
644          <NA>                            <NA>
645          <NA>                            <NA>
646          <NA>                            <NA>
647          <NA>                            <NA>
648          <NA>                            <NA>
649          <NA>                            <NA>
650          <NA>                            <NA>
651          <NA>                            <NA>
652          <NA>                            <NA>
653          <NA>                            <NA>
654          <NA>                            <NA>
655          <NA>                            <NA>
656          <NA>                            <NA>
657          <NA>              Grant,  Grant 1965
658          <NA>              Grant,  Grant 1965
659          <NA>              Grant,  Grant 1965
660          <NA>              Grant,  Grant 1965
661          <NA>              Grant,  Grant 1965
662          <NA>              Grant,  Grant 1965
663          <NA>              Grant,  Grant 1965
664          <NA>                            <NA>
665          <NA>                            <NA>
666          <NA>                            <NA>
667          <NA>                            <NA>
668          <NA>                            <NA>
669          <NA>                            <NA>
670          <NA>                            <NA>
671          <NA>                            <NA>
672          <NA>                            <NA>
673          <NA>                            <NA>
674          <NA>                            <NA>
675          <NA>                            <NA>
676          <NA>                            <NA>
677          <NA>                            <NA>
678          <NA>                            <NA>
679          <NA>                            <NA>
680          <NA>                            <NA>
681          <NA>                            <NA>
682          <NA>              Grant,  Grant 1965
683          <NA>              Grant,  Grant 1965
684          <NA>              Grant,  Grant 1965
685          <NA>              Grant,  Grant 1965
686          <NA>              Grant,  Grant 1965
687          <NA>              Grant,  Grant 1965
688          <NA>              Grant,  Grant 1965
689          <NA>              Grant,  Grant 1965
690          <NA>    Arizona-Sonora Desert Museum
691          <NA>    Arizona-Sonora Desert Museum
692          <NA>    Arizona-Sonora Desert Museum
693          <NA>    Arizona-Sonora Desert Museum
694          <NA>    Arizona-Sonora Desert Museum
695          <NA>    Arizona-Sonora Desert Museum
696          <NA>    Arizona-Sonora Desert Museum
697          <NA>    Arizona-Sonora Desert Museum
698          <NA>    Arizona-Sonora Desert Museum
699          <NA>    Arizona-Sonora Desert Museum
700          <NA>              Grant,  Grant 1965
701          <NA>              Grant,  Grant 1965
702          <NA>              Grant,  Grant 1965
703          <NA>              Grant,  Grant 1965
704          <NA>              Grant,  Grant 1965
705          <NA>              Grant,  Grant 1965
706          <NA>              Grant,  Grant 1965
707          <NA>              Grant,  Grant 1965
708          <NA>              Grant,  Grant 1965
709          <NA>              Grant,  Grant 1965
710          <NA>              Grant,  Grant 1965
711          <NA>              Grant,  Grant 1965
712          <NA>              Grant,  Grant 1965
713          <NA>              Grant,  Grant 1965
714          <NA>              Grant,  Grant 1965
715          <NA>              Grant,  Grant 1965
716          <NA>              Grant,  Grant 1965
717          <NA>              Grant,  Grant 1965
718          <NA>              Grant,  Grant 1965
719          <NA>              Grant,  Grant 1965
720          <NA>              Grant,  Grant 1965
721          <NA>              Grant,  Grant 1965
722          <NA>              Grant,  Grant 1965
723          <NA>              Grant,  Grant 1965
724          <NA>              Grant,  Grant 1965
725          <NA>              Grant,  Grant 1965
726          <NA>              Grant,  Grant 1965
727          <NA>              Grant,  Grant 1965
728          <NA>              Grant,  Grant 1965
729          <NA>              Grant,  Grant 1965
730          <NA>              Grant,  Grant 1965
731          <NA>              Grant,  Grant 1965
732          <NA>              Grant,  Grant 1965
733          <NA>              Grant,  Grant 1965
734          <NA>              Grant,  Grant 1965
735          <NA>              Grant,  Grant 1965
736          <NA>              Grant,  Grant 1965
737          <NA>              Grant,  Grant 1965
738          <NA>              Grant,  Grant 1965
739          <NA>              Grant,  Grant 1965
740          <NA>              Grant,  Grant 1965
741          <NA>              Grant,  Grant 1965
742          <NA>              Grant,  Grant 1965
743          <NA>              Grant,  Grant 1965
744          <NA>              Grant,  Grant 1965
745          <NA>              Grant,  Grant 1965
746          <NA>              Grant,  Grant 1965
747          <NA>              Grant,  Grant 1965
748          <NA>              Grant,  Grant 1965
749          <NA>              Grant,  Grant 1965
750          <NA>              Grant,  Grant 1965
751          <NA>              Grant,  Grant 1965
752          <NA>              Grant,  Grant 1965
753          <NA>              Grant,  Grant 1965
754          <NA>              Grant,  Grant 1965
755          <NA>              Grant,  Grant 1965
756          <NA>              Grant,  Grant 1965
757          <NA>              Grant,  Grant 1965
758          <NA>              Grant,  Grant 1965
759          <NA>              Grant,  Grant 1965
760          <NA>              Grant,  Grant 1965
761          <NA>              Grant,  Grant 1965
762          <NA>              Grant,  Grant 1965
763          <NA>              Grant,  Grant 1965
764          <NA>              Grant,  Grant 1965
765          <NA>              Grant,  Grant 1965
766          <NA>              Grant,  Grant 1965
767          <NA>              Grant,  Grant 1965
768          <NA>              Grant,  Grant 1965
769          <NA>              Grant,  Grant 1965
770          <NA>              Grant,  Grant 1965
771          <NA>              Grant,  Grant 1965
772          <NA>              Grant,  Grant 1965
773          <NA>              Grant,  Grant 1965
774          <NA>              Grant,  Grant 1965
775          <NA>              Grant,  Grant 1965
776          <NA>              Grant,  Grant 1965
777          <NA>              Grant,  Grant 1965
778          <NA>              Grant,  Grant 1965
779          <NA>              Grant,  Grant 1965
780          <NA>              Grant,  Grant 1965
781          <NA>              Grant,  Grant 1965
782          <NA>              Grant,  Grant 1965
783          <NA>              Grant,  Grant 1965
784          <NA>              Grant,  Grant 1965
785          <NA>              Grant,  Grant 1965
786          <NA>              Grant,  Grant 1965
787          <NA>              Grant,  Grant 1965
788          <NA>              Grant,  Grant 1965
789          <NA>              Grant,  Grant 1965
790          <NA>              Grant,  Grant 1965
791          <NA>              Grant,  Grant 1965
792          <NA>              Grant,  Grant 1965
793          <NA>              Grant,  Grant 1965
794          <NA>              Grant,  Grant 1965
795          <NA>              Grant,  Grant 1965
796          <NA>              Grant,  Grant 1965
797          <NA>              Grant,  Grant 1965
798          <NA>              Grant,  Grant 1965
799          <NA>              Grant,  Grant 1965
800          <NA>              Grant,  Grant 1965
801          <NA>              Grant,  Grant 1965
802          <NA>              Grant,  Grant 1965
803          <NA>              Grant,  Grant 1965
804          <NA>              Grant,  Grant 1965
805          <NA>              Grant,  Grant 1965
806          <NA>              Grant,  Grant 1965
807          <NA>              Grant,  Grant 1965
808          <NA>              Grant,  Grant 1965
809          <NA>              Grant,  Grant 1965
810          <NA>              Grant,  Grant 1965
811          <NA>              Grant,  Grant 1965
812          <NA>              Grant,  Grant 1965
813          <NA>              Grant,  Grant 1965
814          <NA>              Grant,  Grant 1965
815          <NA>              Grant,  Grant 1965
816          <NA>              Grant,  Grant 1965
817          <NA>              Grant,  Grant 1965
818          <NA>              Grant,  Grant 1965
819          <NA>              Grant,  Grant 1965
820          <NA>              Grant,  Grant 1965
821          <NA>              Grant,  Grant 1965
822          <NA>              Grant,  Grant 1965
823          <NA>              Grant,  Grant 1965
824          <NA>              Grant,  Grant 1965
825          <NA>              Grant,  Grant 1965
826          <NA>              Grant,  Grant 1965
827          <NA>              Grant,  Grant 1965
828          <NA>              Grant,  Grant 1965
829          <NA>              Grant,  Grant 1965
830          <NA>              Grant,  Grant 1965
831          <NA>              Grant,  Grant 1965
832          <NA>              Grant,  Grant 1965
833          <NA>              Grant,  Grant 1965
834          <NA>              Grant,  Grant 1965
835          <NA>              Grant,  Grant 1965
836          <NA>              Grant,  Grant 1965
837          <NA>              Grant,  Grant 1965
838          <NA>              Grant,  Grant 1965
839          <NA>              Grant,  Grant 1965
840          <NA>              Grant,  Grant 1965
841          <NA>              Grant,  Grant 1965
842          <NA>              Grant,  Grant 1965
843          <NA>              Grant,  Grant 1965
844          <NA>              Grant,  Grant 1965
845          <NA>              Grant,  Grant 1965
846          <NA>              Grant,  Grant 1965
847          <NA>              Grant,  Grant 1965
848          <NA>              Grant,  Grant 1965
849          <NA>              Grant,  Grant 1965
850          <NA>              Grant,  Grant 1965
851          <NA>              Grant,  Grant 1965
852          <NA>              Grant,  Grant 1965
853          <NA>              Grant,  Grant 1965
854          <NA>              Grant,  Grant 1965
855          <NA>              Grant,  Grant 1965
856          <NA>              Grant,  Grant 1965
857          <NA>              Grant,  Grant 1965
858          <NA>              Grant,  Grant 1965
859          <NA>              Grant,  Grant 1965
860          <NA>              Grant,  Grant 1965
861          <NA>              Grant,  Grant 1965
862          <NA>              Grant,  Grant 1965
863          <NA>              Grant,  Grant 1965
864          <NA>              Grant,  Grant 1965
865          <NA>              Grant,  Grant 1965
866          <NA>              Grant,  Grant 1965
867          <NA>                            <NA>
868          <NA>                            <NA>
869          <NA>                            <NA>
870          <NA>                            <NA>
871          <NA>                            <NA>
872          <NA>                            <NA>
873          <NA>                            <NA>
874          <NA>                            <NA>
875          <NA>                            <NA>
876          <NA>                            <NA>
877          <NA>                            <NA>
878          <NA>                            <NA>
879          <NA>                            <NA>
880          <NA>                            <NA>
881          <NA>              Grant,  Grant 1965
882          <NA>              Grant,  Grant 1965
883          <NA>              Grant,  Grant 1965
884          <NA>              Grant,  Grant 1965
885          <NA>              Grant,  Grant 1965
886          <NA>              Grant,  Grant 1965
887          <NA>              Grant,  Grant 1965
888          <NA>              Grant,  Grant 1965
889          <NA>              Grant,  Grant 1965
890          <NA>              Grant,  Grant 1965
891          <NA>              Grant,  Grant 1965
892          <NA>              Grant,  Grant 1965
893          <NA>              Grant,  Grant 1965
894          <NA>              Grant,  Grant 1965
895          <NA>              Grant,  Grant 1965
896          <NA>              Grant,  Grant 1965
897          <NA>              Grant,  Grant 1965
898          <NA>              Grant,  Grant 1965
899          <NA>              Grant,  Grant 1965
900          <NA>              Grant,  Grant 1965
901          <NA>              Grant,  Grant 1965
902          <NA>              Grant,  Grant 1965
903          <NA>              Grant,  Grant 1965
904          <NA>              Grant,  Grant 1965
905          <NA>              Grant,  Grant 1965
906          <NA>              Grant,  Grant 1965
907          <NA>              Grant,  Grant 1965
908          <NA>              Grant,  Grant 1965
909          <NA>              Grant,  Grant 1965
910          <NA>              Grant,  Grant 1965
911          <NA>              Grant,  Grant 1965
912          <NA>              Grant,  Grant 1965
913          <NA>              Grant,  Grant 1965
914          <NA>              Grant,  Grant 1965
915          <NA>              Grant,  Grant 1965
916          <NA>              Grant,  Grant 1965
917          <NA>              Grant,  Grant 1965
918          <NA>              Grant,  Grant 1965
919          <NA>              Grant,  Grant 1965
920          <NA>              Grant,  Grant 1965
921          <NA>              Grant,  Grant 1965
922          <NA>              Grant,  Grant 1965
923          <NA>              Grant,  Grant 1965
924          <NA>              Grant,  Grant 1965
925          <NA>              Grant,  Grant 1965
926          <NA>              Grant,  Grant 1965
927          <NA>              Grant,  Grant 1965
928          <NA>              Grant,  Grant 1965
929          <NA>              Grant,  Grant 1965
930          <NA>              Grant,  Grant 1965
931          <NA>              Grant,  Grant 1965
932          <NA>              Grant,  Grant 1965
933          <NA>              Grant,  Grant 1965
934          <NA>              Grant,  Grant 1965
935          <NA>              Grant,  Grant 1965
936          <NA>              Grant,  Grant 1965
937          <NA>              Grant,  Grant 1965
938          <NA>              Grant,  Grant 1965
939          <NA>              Grant,  Grant 1965
940          <NA>              Grant,  Grant 1965
941          <NA>              Grant,  Grant 1965
942          <NA>              Grant,  Grant 1965
943          <NA>              Grant,  Grant 1965
944          <NA>              Grant,  Grant 1965
945          <NA>              Grant,  Grant 1965
946          <NA>              Grant,  Grant 1965
947          <NA>              Grant,  Grant 1965
948          <NA>              Grant,  Grant 1965
949          <NA>              Grant,  Grant 1965
950          <NA>              Grant,  Grant 1965
951          <NA>              Grant,  Grant 1965
952          <NA>              Grant,  Grant 1965
953          <NA>              Grant,  Grant 1965
954          <NA>              Grant,  Grant 1965
955          <NA>              Grant,  Grant 1965
956          <NA>              Grant,  Grant 1965
957          <NA>              Grant,  Grant 1965
958          <NA>              Grant,  Grant 1965
959          <NA>              Grant,  Grant 1965
960          <NA>              Grant,  Grant 1965
961          <NA>              Grant,  Grant 1965
962          <NA>              Grant,  Grant 1965
963          <NA>              Grant,  Grant 1965
964          <NA>              Grant,  Grant 1965
965          <NA>              Grant,  Grant 1965
966          <NA>              Grant,  Grant 1965
967          <NA>              Grant,  Grant 1965
968          <NA>              Grant,  Grant 1965
969          <NA>              Grant,  Grant 1965
970          <NA>              Grant,  Grant 1965
971          <NA>              Grant,  Grant 1965
972          <NA>              Grant,  Grant 1965
973          <NA>              Grant,  Grant 1965
974          <NA>              Grant,  Grant 1965
975          <NA>              Grant,  Grant 1965
976          <NA>              Grant,  Grant 1965
977          <NA>              Grant,  Grant 1965
978          <NA>              Grant,  Grant 1965
979          <NA>              Grant,  Grant 1965
980          <NA>              Grant,  Grant 1965
981          <NA>              Grant,  Grant 1965
982          <NA>              Grant,  Grant 1965
983          <NA>              Grant,  Grant 1965
984          <NA>              Grant,  Grant 1965
985          <NA>              Grant,  Grant 1965
986          <NA>              Grant,  Grant 1965
987          <NA>              Grant,  Grant 1965
988          <NA>              Grant,  Grant 1965
989          <NA>              Grant,  Grant 1965
990          <NA>                            <NA>
991          <NA>                            <NA>
992          <NA>                            <NA>
993          <NA>                            <NA>
994          <NA>                            <NA>
995          <NA>                            <NA>
996          <NA>                            <NA>
997          <NA>                            <NA>
998          <NA>                            <NA>
999          <NA>                            <NA>
1000         <NA>                            <NA>
1001         <NA>                            <NA>
1002         <NA>                            <NA>
1003         <NA>                            <NA>
1004         <NA>              Grant,  Grant 1965
1005         <NA>              Grant,  Grant 1965
1006         <NA>              Grant,  Grant 1965
1007         <NA>              Grant,  Grant 1965
1008         <NA>              Grant,  Grant 1965
1009         <NA>              Grant,  Grant 1965
1010         <NA>              Grant,  Grant 1965
1011         <NA>              Grant,  Grant 1965
1012         <NA>              Grant,  Grant 1965
1013         <NA>              Grant,  Grant 1965
1014         <NA>              Grant,  Grant 1965
1015         <NA>                            <NA>
1016         <NA>                            <NA>
1017         <NA>                            <NA>
1018         <NA>                            <NA>
1019         <NA>                            <NA>
1020         <NA>                            <NA>
1021         <NA>                            <NA>
1022         <NA>                            <NA>
1023         <NA>                            <NA>
1024         <NA>                            <NA>
1025         <NA>              Grant,  Grant 1965
1026         <NA>              Grant,  Grant 1965
1027         <NA>              Grant,  Grant 1965
1028         <NA>              Grant,  Grant 1965
1029         <NA>              Grant,  Grant 1965
1030         <NA>              Grant,  Grant 1965
1031         <NA>              Grant,  Grant 1965
1032         <NA>              Grant,  Grant 1965
1033         <NA>              Grant,  Grant 1965
1034         <NA>              Grant,  Grant 1965
1035         <NA>              Grant,  Grant 1965
1036         <NA>              Grant,  Grant 1965
1037         <NA>              Grant,  Grant 1965
1038         <NA>              Grant,  Grant 1965
1039         <NA>              Grant,  Grant 1965
1040         <NA>              Grant,  Grant 1965
1041         <NA>              Grant,  Grant 1965
1042         <NA>              Grant,  Grant 1965
1043         <NA>              Grant,  Grant 1965
1044         <NA>              Grant,  Grant 1965
1045         <NA>              Grant,  Grant 1965
1046         <NA>              Grant,  Grant 1965
1047         <NA>              Grant,  Grant 1965
1048         <NA>              Grant,  Grant 1965
1049         <NA>              Grant,  Grant 1965
1050         <NA>              Grant,  Grant 1965
1051         <NA>              Grant,  Grant 1965
1052         <NA>              Grant,  Grant 1965
1053         <NA>              Grant,  Grant 1965
1054         <NA>              Grant,  Grant 1965
1055         <NA>              Grant,  Grant 1965
1056         <NA>              Grant,  Grant 1965
1057         <NA>                            <NA>
1058         <NA>                            <NA>
1059         <NA>                            <NA>
1060         <NA>                            <NA>
1061         <NA>                            <NA>
1062         <NA>                            <NA>
1063         <NA>                            <NA>
1064         <NA>              Grant,  Grant 1965
1065         <NA>              Grant,  Grant 1965
1066         <NA>              Grant,  Grant 1965
1067         <NA>              Grant,  Grant 1965
1068         <NA>              Grant,  Grant 1965
1069         <NA>              Grant,  Grant 1965
1070         <NA>              Grant,  Grant 1965
1071         <NA>              Grant,  Grant 1965
1072         <NA>              Grant,  Grant 1965
1073         <NA>              Grant,  Grant 1965
1074         <NA>                            <NA>
1075         <NA>                            <NA>
1076         <NA>                            <NA>
1077         <NA>                            <NA>
1078         <NA>                            <NA>
1079         <NA>                            <NA>
1080         <NA>                            <NA>
1081         <NA>                            <NA>
1082         <NA>                            <NA>
1083         <NA>                            <NA>
1084         <NA>                            <NA>
1085         <NA>                            <NA>
1086         <NA>                            <NA>
1087         <NA>                       Wood 2009
1088         <NA>                       Wood 2009
1089         <NA>                       Wood 2009
1090         <NA>                       Wood 2009
1091         <NA>                       Wood 2009
1092         <NA>                       Wood 2009
1093         <NA>                       Wood 2009
1094         <NA>                       Wood 2009
1095         <NA>                       Wood 2009
1096         <NA>                       Wood 2009
1097         <NA>                            <NA>
1098         <NA>                            <NA>
1099         <NA>                            <NA>
1100         <NA>                            <NA>
1101         <NA>                            <NA>
1102         <NA>                            <NA>
1103         <NA>                            <NA>
1104         <NA>                            <NA>
1105         <NA>                            <NA>
1106         <NA>                            <NA>
1107         <NA>                            <NA>
1108         <NA>                            <NA>
1109         <NA>                            <NA>
1110         <NA>                            <NA>
1111         <NA>                            <NA>
1112         <NA>                            <NA>
1113         <NA>                            <NA>
1114         <NA>                            <NA>
1115         <NA>                            <NA>
1116         <NA>                            <NA>
1117         <NA>              Grant,  Grant 1965
1118         <NA>              Grant,  Grant 1965
1119         <NA>              Grant,  Grant 1965
1120         <NA>              Grant,  Grant 1965
1121         <NA>              Grant,  Grant 1965
1122         <NA>              Grant,  Grant 1965
1123         <NA>              Grant,  Grant 1965
1124         <NA>              Grant,  Grant 1965
1125         <NA>              Grant,  Grant 1965
1126         <NA>              Grant,  Grant 1965
1127         <NA>              Grant,  Grant 1965
1128         <NA>              Grant,  Grant 1965
1129         <NA>              Grant,  Grant 1965
1130         <NA>              Grant,  Grant 1965
1131         <NA>              Grant,  Grant 1965
1132         <NA>              Grant,  Grant 1965
1133         <NA>              Grant,  Grant 1965
1134         <NA>              Grant,  Grant 1965
1135         <NA>              Grant,  Grant 1965
1136         <NA>              Grant,  Grant 1965
1137         <NA>              Grant,  Grant 1965
1138         <NA>              Grant,  Grant 1965
1139         <NA>              Grant,  Grant 1965
1140         <NA>              Grant,  Grant 1965
1141         <NA>              Grant,  Grant 1965
1142         <NA>              Grant,  Grant 1965
1143         <NA>              Grant,  Grant 1965
1144         <NA>              Grant,  Grant 1965
1145         <NA>              Grant,  Grant 1965
1146         <NA>              Grant,  Grant 1965
1147         <NA>              Grant,  Grant 1965
1148         <NA>              Grant,  Grant 1965
1149         <NA>              Grant,  Grant 1965
1150         <NA>              Grant,  Grant 1965
1151         <NA>              Grant,  Grant 1965
1152         <NA>              Grant,  Grant 1965
1153         <NA>              Grant,  Grant 1965
1154         <NA>              Grant,  Grant 1965
1155         <NA>              Grant,  Grant 1965
1156         <NA>              Grant,  Grant 1965
1157         <NA>              Grant,  Grant 1965
1158         <NA>                            <NA>
1159         <NA>                            <NA>
1160         <NA>                            <NA>
1161         <NA>                            <NA>
1162         <NA>                            <NA>
1163         <NA>                            <NA>
1164         <NA>                            <NA>
1165         <NA>              Grant,  Grant 1965
1166         <NA>              Grant,  Grant 1965
1167         <NA>              Grant,  Grant 1965
1168         <NA>              Grant,  Grant 1965
1169         <NA>              Grant,  Grant 1965
1170         <NA>              Grant,  Grant 1965
1171         <NA>              Grant,  Grant 1965
1172         <NA>              Grant,  Grant 1965
1173         <NA>              Grant,  Grant 1965
1174         <NA>              Grant,  Grant 1965
1175         <NA>              Grant,  Grant 1965
1176         <NA>              Grant,  Grant 1965
1177         <NA>              Grant,  Grant 1965
1178         <NA>              Grant,  Grant 1965
1179         <NA>              Grant,  Grant 1965
1180         <NA>              Grant,  Grant 1965
1181         <NA>              Grant,  Grant 1965
1182         <NA>              Grant,  Grant 1965
1183         <NA>              Grant,  Grant 1965
1184         <NA>              Grant,  Grant 1965
1185         <NA>              Grant,  Grant 1965
1186         <NA>              Grant,  Grant 1965
1187         <NA>              Grant,  Grant 1965
1188         <NA>              Grant,  Grant 1965
1189         <NA>              Grant,  Grant 1965
1190         <NA>              Grant,  Grant 1965
1191         <NA>              Grant,  Grant 1965
1192         <NA>              Grant,  Grant 1965
1193         <NA>              Grant,  Grant 1965
1194         <NA>              Grant,  Grant 1965
1195         <NA>              Grant,  Grant 1965
1196         <NA>              Grant,  Grant 1965
1197         <NA>              Grant,  Grant 1965
1198         <NA>              Grant,  Grant 1965
1199         <NA>              Grant,  Grant 1965
1200         <NA>              Grant,  Grant 1965
1201         <NA>              Grant,  Grant 1965
1202         <NA>              Grant,  Grant 1965
1203         <NA>              Grant,  Grant 1965
1204         <NA>              Grant,  Grant 1965
1205         <NA>              Grant,  Grant 1965
1206         <NA>              Grant,  Grant 1965
1207         <NA>              Grant,  Grant 1965
1208         <NA>              Grant,  Grant 1965
1209         <NA>              Grant,  Grant 1965
1210         <NA>              Grant,  Grant 1965
1211         <NA>              Grant,  Grant 1965
1212         <NA>              Grant,  Grant 1965
1213         <NA>              Grant,  Grant 1965
1214         <NA>              Grant,  Grant 1965
1215         <NA>              Grant,  Grant 1965
1216         <NA>              Grant,  Grant 1965
1217         <NA>              Grant,  Grant 1965
1218         <NA>                            <NA>
1219         <NA>                            <NA>
1220         <NA>                            <NA>
1221         <NA>                            <NA>
1222         <NA>                            <NA>
1223         <NA>                            <NA>
1224         <NA>                            <NA>
1225         <NA>                            <NA>
1226         <NA>                            <NA>
1227         <NA>                            <NA>
1228         <NA>              Grant,  Grant 1965
1229         <NA>              Grant,  Grant 1965
1230         <NA>              Grant,  Grant 1965
1231         <NA>              Grant,  Grant 1965
1232         <NA>              Grant,  Grant 1965
1233         <NA>              Grant,  Grant 1965
1234         <NA>              Grant,  Grant 1965
1235         <NA>              Grant,  Grant 1965
1236         <NA>              Grant,  Grant 1965
1237         <NA>              Grant,  Grant 1965
1238         <NA>              Grant,  Grant 1965
1239         <NA>              Grant,  Grant 1965
1240         <NA>              Grant,  Grant 1965
1241         <NA>              Grant,  Grant 1965
1242         <NA>              Grant,  Grant 1965
1243         <NA>              Grant,  Grant 1965
1244         <NA>              Grant,  Grant 1965
1245         <NA>              Grant,  Grant 1965
1246         <NA>              Grant,  Grant 1965
1247         <NA>              Grant,  Grant 1965
1248         <NA>              Grant,  Grant 1965
1249         <NA>              Grant,  Grant 1965
1250         <NA>              Grant,  Grant 1965
1251         <NA>              Grant,  Grant 1965
1252         <NA>              Grant,  Grant 1965
1253         <NA>              Grant,  Grant 1965
1254         <NA>              Grant,  Grant 1965
1255         <NA>              Grant,  Grant 1965
1256         <NA>              Grant,  Grant 1965
1257         <NA>              Grant,  Grant 1965
1258         <NA>              Grant,  Grant 1965
1259         <NA>              Grant,  Grant 1965
1260         <NA>              Grant,  Grant 1965
1261         <NA>              Grant,  Grant 1965
1262         <NA>              Grant,  Grant 1965
1263         <NA>              Grant,  Grant 1965
1264         <NA>              Grant,  Grant 1965
1265         <NA>              Grant,  Grant 1965
1266         <NA>              Grant,  Grant 1965
1267         <NA>              Grant,  Grant 1965
1268         <NA>              Grant,  Grant 1965
1269         <NA>              Grant,  Grant 1965
1270         <NA>              Grant,  Grant 1965
1271         <NA>              Grant,  Grant 1965
1272         <NA>              Grant,  Grant 1965
1273         <NA>              Grant,  Grant 1965
1274         <NA>              Grant,  Grant 1965
1275         <NA>              Grant,  Grant 1965
1276         <NA>                            <NA>
1277         <NA>                            <NA>
1278         <NA>                            <NA>
1279         <NA>                            <NA>
1280         <NA>                            <NA>
1281         <NA>                            <NA>
1282         <NA>              Grant,  Grant 1965
1283         <NA>              Grant,  Grant 1965
1284         <NA>              Grant,  Grant 1965
1285         <NA>              Grant,  Grant 1965
1286         <NA>              Grant,  Grant 1965
1287         <NA>              Grant,  Grant 1965
1288         <NA>              Grant,  Grant 1965
1289         <NA>              Grant,  Grant 1965
1290         <NA>              Grant,  Grant 1965
1291         <NA>              Grant,  Grant 1965
1292         <NA>              Grant,  Grant 1965
1293         <NA>              Grant,  Grant 1965
1294         <NA>              Grant,  Grant 1965
1295         <NA>              Grant,  Grant 1965
1296         <NA>              Grant,  Grant 1965
1297         <NA>              Grant,  Grant 1965
1298         <NA>              Grant,  Grant 1965
1299         <NA>              Grant,  Grant 1965
1300         <NA>              Grant,  Grant 1965
1301         <NA>              Grant,  Grant 1965
1302         <NA>              Grant,  Grant 1965
1303         <NA>              Grant,  Grant 1965
1304         <NA>              Grant,  Grant 1965
1305         <NA>              Grant,  Grant 1965
1306         <NA>              Grant,  Grant 1965
1307         <NA>                            <NA>
1308         <NA>                            <NA>
1309         <NA>                            <NA>
1310         <NA>                            <NA>
1311         <NA>                            <NA>
1312         <NA>                            <NA>
1313         <NA>                            <NA>
1314         <NA>                            <NA>
1315         <NA>                            <NA>
1316         <NA>                            <NA>
1317         <NA>                            <NA>
1318         <NA>                            <NA>
1319         <NA>                            <NA>
1320         <NA>                            <NA>
1321         <NA>                            <NA>
1322         <NA>                            <NA>
1323         <NA>                            <NA>
1324         <NA>                            <NA>
1325         <NA>                            <NA>
1326         <NA>                            <NA>
1327         <NA>              Grant,  Grant 1965
1328         <NA>              Grant,  Grant 1965
1329         <NA>              Grant,  Grant 1965
1330         <NA>              Grant,  Grant 1965
1331         <NA>              Grant,  Grant 1965
1332         <NA>              Grant,  Grant 1965
1333         <NA>              Grant,  Grant 1965
1334         <NA>              Grant,  Grant 1965
1335         <NA>              Grant,  Grant 1965
1336         <NA>              Grant,  Grant 1965
1337         <NA>              Grant,  Grant 1965
1338         <NA>              Grant,  Grant 1965
1339         <NA>              Grant,  Grant 1965
1340         <NA>              Grant,  Grant 1965
1341         <NA>              Grant,  Grant 1965
1342         <NA>              Grant,  Grant 1965
1343         <NA>              Grant,  Grant 1965
1344         <NA>              Grant,  Grant 1965
1345         <NA>              Grant,  Grant 1965
1346         <NA>              Grant,  Grant 1965
1347         <NA>              Grant,  Grant 1965
1348         <NA>              Grant,  Grant 1965
1349         <NA>              Grant,  Grant 1965
1350         <NA>              Grant,  Grant 1965
1351         <NA>                            <NA>
1352         <NA>                            <NA>
1353         <NA>                            <NA>
1354         <NA>                            <NA>
1355         <NA>                            <NA>
1356         <NA>                            <NA>
1357         <NA>                            <NA>
1358         <NA>                            <NA>
1359         <NA>                            <NA>
1360         <NA>                            <NA>
1361         <NA>                            <NA>
1362         <NA>                            <NA>
1363         <NA>                            <NA>
1364         <NA>                            <NA>
1365         <NA>                            <NA>
1366         <NA>                            <NA>
1367         <NA>                            <NA>
1368         <NA>                            <NA>
1369         <NA>                            <NA>
1370         <NA>                            <NA>
1371         <NA>                            <NA>
1372         <NA>                            <NA>
1373         <NA>                            <NA>
1374         <NA>                            <NA>
1375         <NA>                            <NA>
1376         <NA>                            <NA>
1377         <NA>                            <NA>
1378         <NA>              Grant,  Grant 1965
1379         <NA>              Grant,  Grant 1965
1380         <NA>              Grant,  Grant 1965
1381         <NA>              Grant,  Grant 1965
1382         <NA>              Grant,  Grant 1965
1383         <NA>              Grant,  Grant 1965
1384         <NA>              Grant,  Grant 1965
1385         <NA>              Grant,  Grant 1965
1386         <NA>              Grant,  Grant 1965
1387         <NA>              Grant,  Grant 1965
1388         <NA>                            <NA>
1389         <NA>                            <NA>
1390         <NA>                            <NA>
1391         <NA>                            <NA>
1392         <NA>                            <NA>
1393         <NA>                            <NA>
1394         <NA>                            <NA>
1395         <NA>              Grant,  Grant 1965
1396         <NA>              Grant,  Grant 1965
1397         <NA>              Grant,  Grant 1965
1398         <NA>              Grant,  Grant 1965
1399         <NA>              Grant,  Grant 1965
1400         <NA>              Grant,  Grant 1965
1401         <NA>              Grant,  Grant 1965
1402         <NA>              Grant,  Grant 1965
1403         <NA>              Grant,  Grant 1965
1404         <NA>              Grant,  Grant 1965
1405         <NA>              Grant,  Grant 1965
1406         <NA>              Grant,  Grant 1965
1407         <NA>              Grant,  Grant 1965
1408         <NA>                            <NA>
1409         <NA>                            <NA>
1410         <NA>                            <NA>
1411         <NA>                            <NA>
1412         <NA>              Grant,  Grant 1965
1413         <NA>              Grant,  Grant 1965
1414         <NA>              Grant,  Grant 1965
1415         <NA>              Grant,  Grant 1965
1416         <NA>                            <NA>
1417         <NA>                            <NA>
1418         <NA>                            <NA>
1419         <NA>                            <NA>
1420         <NA>                            <NA>
1421         <NA>                            <NA>
1422         <NA>                            <NA>
1423         <NA>                            <NA>
1424         <NA>                            <NA>
1425         <NA>                            <NA>
1426         <NA>                            <NA>
1427         <NA>                            <NA>
1428         <NA>                            <NA>
1429         <NA>                            <NA>
1430         <NA>                            <NA>
1431         <NA>                            <NA>
1432         <NA>                            <NA>
1433         <NA>                            <NA>
1434         <NA>                            <NA>
1435         <NA>                            <NA>
1436         <NA>                            <NA>
1437         <NA>                            <NA>
1438         <NA>                            <NA>
1439         <NA>              Grant,  Grant 1965
1440         <NA>              Grant,  Grant 1965
1441         <NA>              Grant,  Grant 1965
1442         <NA>              Grant,  Grant 1965
1443         <NA>              Grant,  Grant 1965
1444         <NA>              Grant,  Grant 1965
1445         <NA>              Grant,  Grant 1965
1446         <NA>              Grant,  Grant 1965
1447         <NA>              Grant,  Grant 1965
1448         <NA>              Grant,  Grant 1965
1449         <NA>              Grant,  Grant 1965
1450         <NA>              Grant,  Grant 1965
1451         <NA>              Grant,  Grant 1965
1452         <NA>              Grant,  Grant 1965
1453         <NA>              Grant,  Grant 1965
1454         <NA>              Grant,  Grant 1965
1455         <NA>              Grant,  Grant 1965
1456         <NA>              Grant,  Grant 1965
1457         <NA>                            <NA>
1458         <NA>                            <NA>
1459         <NA>                            <NA>
1460         <NA>                            <NA>
1461         <NA>                            <NA>
1462         <NA>                            <NA>
1463         <NA>                            <NA>
1464         <NA>                            <NA>
1465         <NA>                            <NA>
1466         <NA>                            <NA>
1467         <NA>                            <NA>
1468         <NA>                            <NA>
1469         <NA>                            <NA>
1470         <NA>                            <NA>
1471         <NA>                            <NA>
1472         <NA>                            <NA>
1473         <NA>                            <NA>
1474         <NA>              Grant,  Grant 1965
1475         <NA>              Grant,  Grant 1965
1476         <NA>              Grant,  Grant 1965
1477         <NA>              Grant,  Grant 1965
1478         <NA>                            <NA>
1479         <NA>                            <NA>
1480         <NA>                            <NA>
1481         <NA>                            <NA>
1482         <NA>                            <NA>
1483         <NA>                            <NA>
1484         <NA>                            <NA>
1485         <NA>                            <NA>
1486         <NA>                            <NA>
1487         <NA>                            <NA>
1488         <NA>                            <NA>
1489         <NA>                            <NA>
1490         <NA>                            <NA>
1491         <NA>                            <NA>
1492         <NA>                            <NA>
1493         <NA>                            <NA>
1494         <NA>                            <NA>
1495         <NA>                            <NA>
1496         <NA>              Grant,  Grant 1965
1497         <NA>              Grant,  Grant 1965
1498         <NA>              Grant,  Grant 1965
1499         <NA>              Grant,  Grant 1965
1500         <NA>              Grant,  Grant 1965
1501         <NA>              Grant,  Grant 1965
1502         <NA>              Grant,  Grant 1965
1503         <NA>              Grant,  Grant 1965
1504         <NA>              Grant,  Grant 1965
1505         <NA>              Grant,  Grant 1965
1506         <NA>              Grant,  Grant 1965
1507         <NA>              Grant,  Grant 1965
1508         <NA>              Grant,  Grant 1965
1509         <NA>                            <NA>
1510         <NA>                            <NA>
1511         <NA>                            <NA>
1512         <NA>                            <NA>
1513         <NA>                            <NA>
1514         <NA>                            <NA>
1515         <NA>                            <NA>
1516         <NA>                            <NA>
1517         <NA>                            <NA>
1518         <NA>                            <NA>
1519         <NA>                            <NA>
1520         <NA>              Grant,  Grant 1965
1521         <NA>              Grant,  Grant 1965
1522         <NA>              Grant,  Grant 1965
1523         <NA>              Grant,  Grant 1965
1524         <NA>              Grant,  Grant 1965
1525         <NA>              Grant,  Grant 1965
1526         <NA>              Grant,  Grant 1965
1527         <NA>              Grant,  Grant 1965
1528         <NA>              Grant,  Grant 1965
1529         <NA>              Grant,  Grant 1965
1530         <NA>              Grant,  Grant 1965
1531         <NA>              Grant,  Grant 1965
1532         <NA>              Grant,  Grant 1965
1533         <NA>              Grant,  Grant 1965
1534         <NA>              Grant,  Grant 1965
1535         <NA>              Grant,  Grant 1965
1536         <NA>              Grant,  Grant 1965
1537         <NA>              Grant,  Grant 1965
1538         <NA>              Grant,  Grant 1965
1539         <NA>              Grant,  Grant 1965
1540         <NA>                            <NA>
1541         <NA>                            <NA>
1542         <NA>                            <NA>
1543         <NA>                            <NA>
1544         <NA>                            <NA>
1545         <NA>                            <NA>
1546         <NA>                            <NA>
1547         <NA>                            <NA>
1548         <NA>                            <NA>
1549         <NA>                            <NA>
1550         <NA>                            <NA>
1551         <NA>                            <NA>
1552         <NA>                            <NA>
1553         <NA>                            <NA>
1554         <NA>                            <NA>
1555         <NA>                            <NA>
1556         <NA>                            <NA>
1557         <NA>                            <NA>
1558         <NA>                            <NA>
1559         <NA>                            <NA>
1560         <NA>                            <NA>
1561         <NA>                            <NA>
1562         <NA>                            <NA>
1563        flies              Grant,  Grant 1965
1564        flies              Grant,  Grant 1965
1565        flies              Grant,  Grant 1965
1566        flies              Grant,  Grant 1965
1567        flies              Grant,  Grant 1965
1568        flies              Grant,  Grant 1965
1569        flies              Grant,  Grant 1965
1570        flies              Grant,  Grant 1965
1571        flies              Grant,  Grant 1965
1572        flies              Grant,  Grant 1965
1573        flies              Grant,  Grant 1965
1574        flies              Grant,  Grant 1965
1575        flies              Grant,  Grant 1965
1576        flies              Grant,  Grant 1965
1577        flies              Grant,  Grant 1965
1578        flies              Grant,  Grant 1965
1579        flies              Grant,  Grant 1965
1580        flies              Grant,  Grant 1965
1581        flies              Grant,  Grant 1965
1582         <NA>                            <NA>
1583         <NA>                            <NA>
1584         <NA>                            <NA>
1585         <NA>                            <NA>
1586         <NA>                            <NA>
1587         <NA>                            <NA>
1588         <NA>                            <NA>
1589         <NA>                            <NA>
1590         <NA>                            <NA>
1591         <NA>                            <NA>
1592         <NA>                            <NA>
1593         <NA>                            <NA>
1594         <NA>                            <NA>
1595         <NA>                            <NA>
1596         <NA>                            <NA>
1597         <NA>                            <NA>
1598         <NA>                            <NA>
1599         <NA>                            <NA>
1600         <NA>                            <NA>
1601         <NA>                            <NA>
1602         <NA>                            <NA>
1603         <NA>                            <NA>
1604         <NA>                            <NA>
1605         <NA>                            <NA>
1606         <NA>                            <NA>
1607         <NA>                            <NA>
1608         <NA>                            <NA>
1609         <NA>              Grant,  Grant 1965
1610         <NA>              Grant,  Grant 1965
1611         <NA>              Grant,  Grant 1965
1612         <NA>              Grant,  Grant 1965
1613         <NA>              Grant,  Grant 1965
1614         <NA>              Grant,  Grant 1965
1615         <NA>              Grant,  Grant 1965
1616         <NA>              Grant,  Grant 1965
1617         <NA>              Grant,  Grant 1965
1618         <NA>              Grant,  Grant 1965
1619         <NA>              Grant,  Grant 1965
1620         <NA>              Grant,  Grant 1965
1621         <NA>              Grant,  Grant 1965
1622         <NA>              Grant,  Grant 1965
1623         <NA>              Grant,  Grant 1965
1624         <NA>              Grant,  Grant 1965
1625         <NA>              Grant,  Grant 1965
1626         <NA>              Grant,  Grant 1965
1627         <NA>              Grant,  Grant 1965
1628         <NA>              Grant,  Grant 1965
1629         <NA>              Grant,  Grant 1965
1630         <NA>              Grant,  Grant 1965
1631         <NA>              Grant,  Grant 1965
1632         <NA>                            <NA>
1633         <NA>                            <NA>
1634         <NA>                            <NA>
1635         <NA>                            <NA>
1636         <NA>                            <NA>
1637         <NA>                            <NA>
1638         <NA>                            <NA>
1639         <NA>                            <NA>
1640         <NA>                            <NA>
1641         <NA>                            <NA>
1642         <NA>                            <NA>
1643         <NA>                            <NA>
1644         <NA>                            <NA>
1645         <NA>                            <NA>
1646         <NA>                            <NA>
1647         <NA>                            <NA>
1648         <NA>                            <NA>
1649         <NA>                            <NA>
1650         <NA>                            <NA>
1651         <NA>                            <NA>
1652         <NA>                            <NA>
1653         <NA>                            <NA>
1654         <NA>                            <NA>
1655         <NA>                            <NA>
1656         <NA>                            <NA>
1657         <NA>                            <NA>
1658         <NA>                            <NA>
1659         <NA>                            <NA>
1660         <NA>                            <NA>
1661         <NA>                            <NA>
1662         <NA>                            <NA>
1663         <NA>                            <NA>
1664         <NA>                            <NA>
1665         <NA>                            <NA>
1666         <NA>                            <NA>
1667         <NA>                            <NA>
1668         <NA>                            <NA>
1669         <NA>                            <NA>
1670         <NA>                            <NA>
1671         <NA>                            <NA>
1672         <NA>              Grant,  Grant 1965
1673         <NA>              Grant,  Grant 1965
1674         <NA>              Grant,  Grant 1965
1675         <NA>              Grant,  Grant 1965
1676         <NA>              Grant,  Grant 1965
1677         <NA>              Grant,  Grant 1965
1678         <NA>              Grant,  Grant 1965
1679         <NA>                            <NA>
1680         <NA>                            <NA>
1681         <NA>                            <NA>
1682         <NA>                            <NA>
1683         <NA>                            <NA>
1684         <NA>                            <NA>
1685         <NA>                            <NA>
1686         <NA>                            <NA>
1687         <NA>                            <NA>
1688         <NA>                            <NA>
1689         <NA>                            <NA>
1690         <NA>                            <NA>
1691         <NA>                            <NA>
1692         <NA>                            <NA>
1693         <NA>                            <NA>
1694         <NA>                            <NA>
1695         <NA>                            <NA>
1696         <NA>                            <NA>
1697         <NA>                            <NA>
1698         <NA>                            <NA>
1699         <NA>                            <NA>
1700         <NA>                            <NA>
1701         <NA>                            <NA>
1702         <NA>                            <NA>
1703         <NA>                            <NA>
1704         <NA>                            <NA>
1705         <NA>                            <NA>
1706         <NA>                            <NA>
1707         <NA>                            <NA>
1708         <NA>                            <NA>
1709         <NA>                            <NA>
1710         <NA>                            <NA>
1711         <NA>                            <NA>
1712         <NA>                            <NA>
1713         <NA>                            <NA>
1714         <NA>                            <NA>
1715         <NA>                            <NA>
1716         <NA>                            <NA>
1717         <NA>                            <NA>
1718         <NA>                            <NA>
1719         <NA>                            <NA>
1720         <NA>                            <NA>
1721         <NA>                            <NA>
1722         <NA>                            <NA>
1723         <NA>                            <NA>
1724         <NA>                            <NA>
1725         <NA>                            <NA>
1726         <NA>                            <NA>
1727         <NA>                            <NA>
1728         <NA>                            <NA>
1729         <NA>                            <NA>
1730         <NA>                            <NA>
1731         <NA>                            <NA>
1732         <NA>                            <NA>
1733         <NA>                            <NA>
1734         <NA>                            <NA>
1735         <NA>                            <NA>
1736         <NA>                            <NA>
1737         <NA>                            <NA>
1738         <NA>                            <NA>
1739         <NA>                            <NA>
1740         <NA>                            <NA>
1741         <NA>                            <NA>
1742         <NA>                            <NA>
1743         <NA>                            <NA>
1744         <NA>                            <NA>
1745         <NA>                            <NA>
1746         <NA>                            <NA>
1747         <NA>                            <NA>
1748         <NA>                            <NA>
1749         <NA>                            <NA>
1750         <NA>                            <NA>
1751         <NA>                            <NA>
1752         <NA>                            <NA>
1753         <NA>                            <NA>
1754         <NA>                            <NA>
1755         <NA>                            <NA>
1756         <NA>                            <NA>
1757         <NA>                            <NA>
1758         <NA>                            <NA>
1759         <NA>                            <NA>
1760         <NA>                            <NA>
1761         <NA>                            <NA>
1762         <NA>                            <NA>
1763         <NA>                            <NA>
1764         <NA>                            <NA>
1765         <NA>                            <NA>
1766         <NA>                            <NA>
1767         <NA>                            <NA>
1768         <NA>                            <NA>
1769         <NA>                            <NA>
1770         <NA>                            <NA>
1771         <NA>              Grant,  Grant 1965
1772         <NA>              Grant,  Grant 1965
1773         <NA>              Grant,  Grant 1965
1774         <NA>              Grant,  Grant 1965
1775         <NA>                            <NA>
1776         <NA>                            <NA>
1777         <NA>                            <NA>
1778         <NA>                            <NA>
1779         <NA>                            <NA>
1780         <NA>                            <NA>
1781         <NA>                            <NA>
1782         <NA>                            <NA>
1783         <NA>                            <NA>
1784         <NA>                            <NA>
1785         <NA>                            <NA>
1786         <NA>                            <NA>
1787         <NA>                            <NA>
1788         <NA>                            <NA>
1789         <NA>                            <NA>
1790         <NA>                            <NA>
1791         <NA>                            <NA>
1792         <NA>                            <NA>
1793         <NA>                            <NA>
1794         <NA>                            <NA>
1795         <NA>                            <NA>
1796         <NA>                            <NA>
1797         <NA>                            <NA>
1798         <NA>                            <NA>
1799         <NA>                            <NA>
1800         <NA>                            <NA>
1801         <NA>                            <NA>
1802         <NA>                            <NA>
1803         <NA>                            <NA>
1804         <NA>                            <NA>
1805         <NA>                            <NA>
1806         <NA>                            <NA>
1807         <NA>                            <NA>
1808         <NA>                            <NA>
1809         <NA>                            <NA>
1810         <NA>              Grant,  Grant 1965
1811         <NA>              Grant,  Grant 1965
1812         <NA>              Grant,  Grant 1965
1813         <NA>              Grant,  Grant 1965
1814         <NA>              Grant,  Grant 1965
1815         <NA>              Grant,  Grant 1965
1816         <NA>              Grant,  Grant 1965
1817         <NA>              Grant,  Grant 1965
1818         <NA>              Grant,  Grant 1965
1819         <NA>              Grant,  Grant 1965
1820         <NA>              Grant,  Grant 1965
1821         <NA>              Grant,  Grant 1965
1822         <NA>              Grant,  Grant 1965
1823         <NA>              Grant,  Grant 1965
1824         <NA>              Grant,  Grant 1965
1825         <NA>              Grant,  Grant 1965
1826         <NA>              Grant,  Grant 1965
1827         <NA>              Grant,  Grant 1965
1828         <NA>              Grant,  Grant 1965
1829         <NA>              Grant,  Grant 1965
1830         <NA>              Grant,  Grant 1965
1831         <NA>              Grant,  Grant 1965
1832         <NA>              Grant,  Grant 1965
1833         <NA>                            <NA>
1834         <NA>                            <NA>
1835         <NA>                            <NA>
1836         <NA>                            <NA>
1837         <NA>                            <NA>
1838         <NA>                            <NA>
1839         <NA>                            <NA>
1840         <NA>                            <NA>
1841         <NA>                            <NA>
1842         <NA>              Grant,  Grant 1965
1843         <NA>              Grant,  Grant 1965
1844         <NA>              Grant,  Grant 1965
1845         <NA>              Grant,  Grant 1965
1846         <NA>              Grant,  Grant 1965
1847         <NA>              Grant,  Grant 1965
1848         <NA>              Grant,  Grant 1965
1849         <NA>              Grant,  Grant 1965
1850         <NA>              Grant,  Grant 1965
1851         <NA>              Grant,  Grant 1965
1852         <NA>              Grant,  Grant 1965
1853         <NA>              Grant,  Grant 1965
1854         <NA>              Grant,  Grant 1965
1855         <NA>              Grant,  Grant 1965
1856         <NA>              Grant,  Grant 1965
1857         <NA>              Grant,  Grant 1965
1858         <NA>              Grant,  Grant 1965
1859         <NA>              Grant,  Grant 1965
1860         <NA>              Grant,  Grant 1965
1861         <NA>              Grant,  Grant 1965
1862         <NA>              Grant,  Grant 1965
1863         <NA>              Grant,  Grant 1965
1864         <NA>              Grant,  Grant 1965
1865         <NA>                            <NA>
1866         <NA>                            <NA>
1867         <NA>                            <NA>
1868         <NA>                            <NA>
1869         <NA>                            <NA>
1870         <NA>                            <NA>
1871         <NA>              Grant,  Grant 1965
1872         <NA>              Grant,  Grant 1965
1873         <NA>              Grant,  Grant 1965
1874         <NA>              Grant,  Grant 1965
1875         <NA>              Grant,  Grant 1965
1876         <NA>              Grant,  Grant 1965
1877         <NA>              Grant,  Grant 1965
1878         <NA>                            <NA>
1879         <NA>                            <NA>
1880         <NA>                            <NA>
1881         <NA>                            <NA>
1882         <NA>                            <NA>
1883         <NA>                            <NA>
1884         <NA>                            <NA>
1885         <NA>                            <NA>
1886         <NA>                            <NA>
1887         <NA>                            <NA>
1888         <NA>                            <NA>
1889         <NA>                            <NA>
1890         <NA>                            <NA>
1891         <NA>                            <NA>
1892         <NA>                            <NA>
1893         <NA>              Grant,  Grant 1965
1894         <NA>              Grant,  Grant 1965
1895         <NA>              Grant,  Grant 1965
1896         <NA>                            <NA>
1897         <NA>                            <NA>
1898         <NA>                            <NA>
1899         <NA>                            <NA>
1900         <NA>                            <NA>
1901         <NA>                            <NA>
1902         <NA>                            <NA>
1903         <NA>                            <NA>
1904         <NA>                            <NA>
1905         <NA>                            <NA>
1906         <NA>                            <NA>
1907         <NA>                            <NA>
1908         <NA>                            <NA>
1909         <NA>                            <NA>
1910         <NA>                            <NA>
1911         <NA>                            <NA>
1912         <NA>                            <NA>
1913         <NA>              Grant,  Grant 1965
1914         <NA>              Grant,  Grant 1965
1915         <NA>              Grant,  Grant 1965
1916         <NA>              Grant,  Grant 1965
1917         <NA>              Grant,  Grant 1965
1918         <NA>              Grant,  Grant 1965
1919         <NA>              Grant,  Grant 1965
1920         <NA>              Grant,  Grant 1965
1921         <NA>              Grant,  Grant 1965
1922         <NA>              Grant,  Grant 1965
1923         <NA>              Grant,  Grant 1965
1924         <NA>              Grant,  Grant 1965
1925         <NA>              Grant,  Grant 1965
1926         <NA>                            <NA>
1927         <NA>                            <NA>
1928         <NA>                            <NA>
1929         <NA>                            <NA>
1930         <NA>              Grant,  Grant 1965
1931         <NA>              Grant,  Grant 1965
1932         <NA>              Grant,  Grant 1965
1933         <NA>              Grant,  Grant 1965
1934         <NA>              Grant,  Grant 1965
1935         <NA>              Grant,  Grant 1965
1936         <NA>              Grant,  Grant 1965
1937         <NA>              Grant,  Grant 1965
1938         <NA>              Grant,  Grant 1965
1939         <NA>              Grant,  Grant 1965
1940         <NA>              Grant,  Grant 1965
1941         <NA>                            <NA>
1942         <NA>                            <NA>
1943         <NA>                            <NA>
1944         <NA>                            <NA>
1945         <NA>                            <NA>
1946         <NA>                            <NA>
1947         <NA>                            <NA>
1948         <NA>                            <NA>
1949         <NA>                            <NA>
1950         <NA>                            <NA>
1951         <NA>                            <NA>
1952         <NA>                            <NA>
1953         <NA>                            <NA>
1954         <NA>                            <NA>
1955         <NA>                            <NA>
1956         <NA>              Grant,  Grant 1965
1957         <NA>              Grant,  Grant 1965
1958         <NA>              Grant,  Grant 1965
1959         <NA>              Grant,  Grant 1965
1960         <NA>              Grant,  Grant 1965
1961         <NA>              Grant,  Grant 1965
1962         <NA>              Grant,  Grant 1965
1963         <NA>              Grant,  Grant 1965
1964         <NA>              Grant,  Grant 1965
1965         <NA>              Grant,  Grant 1965
1966         <NA>              Grant,  Grant 1965
1967         <NA>              Grant,  Grant 1965
1968         <NA>              Grant,  Grant 1965
1969         <NA>              Grant,  Grant 1965
1970         <NA>              Grant,  Grant 1965
1971         <NA>              Grant,  Grant 1965
1972         <NA>              Grant,  Grant 1965
1973         <NA>                            <NA>
1974         <NA>                            <NA>
1975         <NA>                            <NA>
1976         <NA>                            <NA>
1977         <NA>                            <NA>
1978         <NA>                            <NA>
1979         <NA>                            <NA>
1980         <NA>              Grant,  Grant 1965
1981         <NA>              Grant,  Grant 1965
1982         <NA>              Grant,  Grant 1965
1983         <NA>              Grant,  Grant 1965
1984         <NA>              Grant,  Grant 1965
1985         <NA>              Grant,  Grant 1965
1986         <NA>              Grant,  Grant 1965
1987         <NA>                            <NA>
1988         <NA>                            <NA>
1989         <NA>                            <NA>
1990         <NA>                            <NA>
1991         <NA>                            <NA>
1992         <NA>                            <NA>
1993         <NA>                            <NA>
1994         <NA>                            <NA>
1995         <NA>                            <NA>
1996         <NA>                            <NA>
1997         <NA>                            <NA>
1998         <NA>                            <NA>
1999         <NA>                            <NA>
2000         <NA>                            <NA>
2001         <NA>                            <NA>
2002         <NA>                            <NA>
2003         <NA>                            <NA>
2004         <NA>                            <NA>
2005         <NA>                            <NA>
2006         <NA>                            <NA>
2007         <NA>                            <NA>
2008         <NA>                            <NA>
2009         <NA>                            <NA>
2010         <NA>                            <NA>
2011         <NA>                            <NA>
2012         <NA>                            <NA>
2013         <NA>                            <NA>
2014         <NA>                            <NA>
2015         <NA>                            <NA>
2016         <NA>                            <NA>
2017         <NA>                            <NA>
2018         <NA>                            <NA>
2019         <NA>                            <NA>
2020         <NA>                            <NA>
2021         <NA>                            <NA>
2022         <NA>                            <NA>
2023         <NA>                            <NA>
2024         <NA>                            <NA>
2025         <NA>                            <NA>
2026         <NA>                            <NA>
2027         <NA>                            <NA>
2028         <NA>                            <NA>
2029         <NA>              Grant,  Grant 1965
2030         <NA>              Grant,  Grant 1965
2031         <NA>              Grant,  Grant 1965
2032         <NA>              Grant,  Grant 1965
2033         <NA>              Grant,  Grant 1965
2034         <NA>              Grant,  Grant 1965
2035         <NA>              Grant,  Grant 1965
2036         <NA>              Grant,  Grant 1965
2037         <NA>              Grant,  Grant 1965
2038         <NA>              Grant,  Grant 1965
2039         <NA>                            <NA>
2040         <NA>                            <NA>
2041         <NA>                            <NA>
2042         <NA>                            <NA>
2043         <NA>                            <NA>
2044         <NA>                            <NA>
2045         <NA>                            <NA>
2046         <NA>                            <NA>
2047         <NA>                            <NA>
2048         <NA>                            <NA>
2049         <NA>                            <NA>
2050         <NA>                            <NA>
2051         <NA>                            <NA>
2052         <NA>                            <NA>
2053         <NA>                            <NA>
2054         <NA>                            <NA>
2055         <NA>                            <NA>
2056         <NA>                            <NA>
2057         <NA>                            <NA>
2058         <NA>                            <NA>
2059         <NA>                            <NA>
2060         <NA>                            <NA>
2061         <NA>                            <NA>
2062         <NA>                            <NA>
2063         <NA>              Grant,  Grant 1965
2064         <NA>              Grant,  Grant 1965
2065         <NA>              Grant,  Grant 1965
2066         <NA>              Grant,  Grant 1965
2067         <NA>              Grant,  Grant 1965
2068         <NA>              Grant,  Grant 1965
2069         <NA>              Grant,  Grant 1965
2070         <NA>                            <NA>
2071         <NA>                            <NA>
2072         <NA>                            <NA>
2073         <NA>                            <NA>
2074         <NA>                            <NA>
2075         <NA>                            <NA>
2076         <NA>                            <NA>
2077         <NA>                            <NA>
2078         <NA>                            <NA>
2079         <NA>                            <NA>
2080         <NA>                            <NA>
2081         <NA>                            <NA>
2082         <NA>                            <NA>
2083         <NA>                            <NA>
2084         <NA>                            <NA>
2085         <NA>                            <NA>
2086         <NA>                            <NA>
2087         <NA>                            <NA>
2088         <NA>                            <NA>
2089         <NA>                            <NA>
2090         <NA>                            <NA>
2091         <NA>                            <NA>
2092         <NA>                            <NA>
2093         <NA>                            <NA>
2094         <NA>              Grant,  Grant 1965
2095         <NA>              Grant,  Grant 1965
2096         <NA>              Grant,  Grant 1965
2097         <NA>              Grant,  Grant 1965
2098         <NA>              Grant,  Grant 1965
2099         <NA>              Grant,  Grant 1965
2100         <NA>              Grant,  Grant 1965
2101         <NA>              Grant,  Grant 1965
2102         <NA>              Grant,  Grant 1965
2103         <NA>              Grant,  Grant 1965
2104         <NA>              Grant,  Grant 1965
2105         <NA>              Grant,  Grant 1965
2106         <NA>              Grant,  Grant 1965
2107         <NA>              Grant,  Grant 1965
2108         <NA>              Grant,  Grant 1965
2109         <NA>              Grant,  Grant 1965
2110         <NA>              Grant,  Grant 1965
2111         <NA>              Grant,  Grant 1965
2112         <NA>              Grant,  Grant 1965
2113         <NA>              Grant,  Grant 1965
2114         <NA>              Grant,  Grant 1965
2115         <NA>              Grant,  Grant 1965
2116         <NA>              Grant,  Grant 1965
2117         <NA>              Grant,  Grant 1965
2118         <NA>              Grant,  Grant 1965
2119         <NA>              Grant,  Grant 1965
2120         <NA>                            <NA>
2121         <NA>                            <NA>
2122         <NA>                            <NA>
2123         <NA>                            <NA>
2124         <NA>                            <NA>
2125         <NA>              Grant,  Grant 1965
2126         <NA>              Grant,  Grant 1965
2127         <NA>              Grant,  Grant 1965
2128         <NA>              Grant,  Grant 1965
2129         <NA>              Grant,  Grant 1965
2130         <NA>              Grant,  Grant 1965
2131         <NA>              Grant,  Grant 1965
2132         <NA>              Grant,  Grant 1965
2133         <NA>              Grant,  Grant 1965
2134         <NA>              Grant,  Grant 1965
2135         <NA>              Grant,  Grant 1965
2136         <NA>              Grant,  Grant 1965
2137         <NA>              Grant,  Grant 1965
2138         <NA>              Grant,  Grant 1965
2139         <NA>              Grant,  Grant 1965
2140         <NA>              Grant,  Grant 1965
2141         <NA>              Grant,  Grant 1965
2142         <NA>              Grant,  Grant 1965
2143         <NA>              Grant,  Grant 1965
2144         <NA>              Grant,  Grant 1965
2145         <NA>              Grant,  Grant 1965
2146         <NA>              Grant,  Grant 1965
2147         <NA>              Grant,  Grant 1965
2148         <NA>              Grant,  Grant 1965
2149         <NA>              Grant,  Grant 1965
2150         <NA>              Grant,  Grant 1965
2151         <NA>              Grant,  Grant 1965
2152         <NA>              Grant,  Grant 1965
2153         <NA>              Grant,  Grant 1965
2154         <NA>              Grant,  Grant 1965
2155         <NA>              Grant,  Grant 1965
2156         <NA>                            <NA>
2157         <NA>                            <NA>
2158         <NA>                            <NA>
2159         <NA>                            <NA>
2160         <NA>                            <NA>
2161         <NA>                            <NA>
2162         <NA>                            <NA>
2163         <NA>                            <NA>
2164         <NA>                            <NA>
2165         <NA>                            <NA>
2166         <NA>                            <NA>
2167         <NA>                            <NA>
2168         <NA>                            <NA>
2169         <NA>                            <NA>
2170         <NA>                            <NA>
2171         <NA>                            <NA>
2172         <NA>                            <NA>
2173         <NA>                            <NA>
2174         <NA>                            <NA>
2175         <NA>                            <NA>
2176         <NA>                            <NA>
2177         <NA>                            <NA>
2178         <NA>                            <NA>
2179         <NA>                            <NA>
2180         <NA>                            <NA>
2181         <NA>                            <NA>
2182         <NA>                            <NA>
2183         <NA>                            <NA>
2184         <NA>                            <NA>
2185         <NA>                            <NA>
2186         <NA>                            <NA>
2187         <NA>                            <NA>
2188         <NA>                            <NA>
2189         <NA>                            <NA>
2190         <NA>                            <NA>
2191         <NA>                            <NA>
2192         <NA>                            <NA>
2193         <NA>                            <NA>
2194         <NA>                            <NA>
2195         <NA>                            <NA>
2196         <NA>                            <NA>
2197         <NA>                            <NA>
2198         <NA>                            <NA>
2199         <NA>                            <NA>
2200         <NA>                            <NA>
2201         <NA>                            <NA>
2202         <NA>              Grant,  Grant 1965
2203         <NA>              Grant,  Grant 1965
2204         <NA>              Grant,  Grant 1965
2205         <NA>              Grant,  Grant 1965
2206         <NA>              Grant,  Grant 1965
2207         <NA>              Grant,  Grant 1965
2208         <NA>              Grant,  Grant 1965
2209         <NA>              Grant,  Grant 1965
2210         <NA>              Grant,  Grant 1965
2211         <NA>              Grant,  Grant 1965
2212         <NA>              Grant,  Grant 1965
2213         <NA>              Grant,  Grant 1965
2214         <NA>              Grant,  Grant 1965
2215         <NA>              Grant,  Grant 1965
2216         <NA>              Grant,  Grant 1965
2217         <NA>              Grant,  Grant 1965
2218         <NA>                            <NA>
2219         <NA>                            <NA>
2220         <NA>                            <NA>
2221         <NA>                            <NA>
2222         <NA>                            <NA>
2223         <NA>                            <NA>
2224         <NA>                            <NA>
2225         <NA>                            <NA>
2226         <NA>                            <NA>
2227         <NA>                            <NA>
2228         <NA>                            <NA>
2229         <NA>                            <NA>
2230         <NA>                            <NA>
2231         <NA>                            <NA>
2232         <NA>                            <NA>
2233         <NA>              Grant,  Grant 1965
2234         <NA>              Grant,  Grant 1965
2235         <NA>              Grant,  Grant 1965
2236         <NA>              Grant,  Grant 1965
2237         <NA>              Grant,  Grant 1965
2238         <NA>              Grant,  Grant 1965
2239         <NA>              Grant,  Grant 1965
2240         <NA>              Grant,  Grant 1965
2241         <NA>              Grant,  Grant 1965
2242         <NA>              Grant,  Grant 1965
2243         <NA>              Grant,  Grant 1965
2244         <NA>              Grant,  Grant 1965
2245         <NA>              Grant,  Grant 1965
2246         <NA>              Grant,  Grant 1965
2247         <NA>              Grant,  Grant 1965
2248         <NA>              Grant,  Grant 1965
2249         <NA>              Grant,  Grant 1965
2250         <NA>              Grant,  Grant 1965
2251         <NA>              Grant,  Grant 1965
2252         <NA>              Grant,  Grant 1965
2253         <NA>              Grant,  Grant 1965
2254         <NA>              Grant,  Grant 1965
2255         <NA>              Grant,  Grant 1965
2256         <NA>              Grant,  Grant 1965
2257         <NA>              Grant,  Grant 1965
2258         <NA>              Grant,  Grant 1965
2259         <NA>                            <NA>
2260         <NA>                            <NA>
2261         <NA>                            <NA>
2262         <NA>                            <NA>
2263         <NA>                            <NA>
2264         <NA>                            <NA>
2265         <NA>                            <NA>
2266         <NA>                            <NA>
2267         <NA>                            <NA>
2268         <NA>                            <NA>
2269         <NA>              Grant,  Grant 1965
2270         <NA>              Grant,  Grant 1965
2271         <NA>              Grant,  Grant 1965
2272         <NA>              Grant,  Grant 1965
2273         <NA>              Grant,  Grant 1965
2274         <NA>              Grant,  Grant 1965
2275         <NA>              Grant,  Grant 1965
2276         <NA>              Grant,  Grant 1965
2277         <NA>              Grant,  Grant 1965
2278         <NA>              Grant,  Grant 1965
2279         <NA>                            <NA>
2280         <NA>                            <NA>
2281         <NA>                            <NA>
2282         <NA>                            <NA>
2283         <NA>                            <NA>
2284         <NA>                            <NA>
2285         <NA>                            <NA>
2286         <NA>                            <NA>
2287         <NA>                            <NA>
2288         <NA>                            <NA>
2289         <NA>                            <NA>
2290         <NA>                            <NA>
2291         <NA>                            <NA>
2292         <NA>                            <NA>
2293         <NA>                            <NA>
2294         <NA>                            <NA>
2295         <NA>                            <NA>
2296         <NA>                            <NA>
2297         <NA>                            <NA>
2298         <NA>                            <NA>
2299         <NA>                            <NA>
2300         <NA>                            <NA>
2301         <NA>                            <NA>
2302         <NA>                            <NA>
2303         <NA>                            <NA>
2304         <NA>                            <NA>
2305         <NA>                            <NA>
2306         <NA>                            <NA>
2307         <NA>                            <NA>
2308         <NA>                            <NA>
2309         <NA>                            <NA>
2310         <NA>                            <NA>
2311         <NA>                            <NA>
2312         <NA>                            <NA>
2313         <NA>                            <NA>
2314         <NA>                            <NA>
2315         <NA>                            <NA>
2316         <NA>                            <NA>
2317         <NA>                            <NA>
2318         <NA>                            <NA>
2319         <NA>                            <NA>
2320         <NA>                            <NA>
2321         <NA>                            <NA>
2322         <NA>              Grant,  Grant 1965
2323         <NA>              Grant,  Grant 1965
2324         <NA>              Grant,  Grant 1965
2325         <NA>              Grant,  Grant 1965
2326         <NA>              Grant,  Grant 1965
2327         <NA>              Grant,  Grant 1965
2328         <NA>              Grant,  Grant 1965
2329         <NA>              Grant,  Grant 1965
2330         <NA>              Grant,  Grant 1965
2331         <NA>              Grant,  Grant 1965
2332         <NA>              Grant,  Grant 1965
2333         <NA>              Grant,  Grant 1965
2334         <NA>              Grant,  Grant 1965
2335         <NA>              Grant,  Grant 1965
2336         <NA>              Grant,  Grant 1965
2337         <NA>              Grant,  Grant 1965
2338         <NA>              Grant,  Grant 1965
2339         <NA>              Grant,  Grant 1965
2340         <NA>              Grant,  Grant 1965
2341         <NA>                            <NA>
2342         <NA>                            <NA>
2343         <NA>                            <NA>
2344         <NA>              Grant,  Grant 1965
2345         <NA>              Grant,  Grant 1965
2346         <NA>              Grant,  Grant 1965
2347         <NA>              Grant,  Grant 1965
2348         <NA>              Grant,  Grant 1965
2349         <NA>              Grant,  Grant 1965
2350         <NA>              Grant,  Grant 1965
2351         <NA>              Grant,  Grant 1965
2352         <NA>              Grant,  Grant 1965
2353         <NA>              Grant,  Grant 1965
2354         <NA>                            <NA>
2355         <NA>                            <NA>
2356         <NA>                            <NA>
2357         <NA>                            <NA>
2358         <NA>                            <NA>
2359         <NA>                            <NA>
2360         <NA>                            <NA>
2361         <NA>                            <NA>
2362         <NA>                            <NA>
2363         <NA>                            <NA>
2364         <NA>                            <NA>
2365         <NA>                            <NA>
2366         <NA>                            <NA>
2367         <NA>                            <NA>
2368         <NA>                            <NA>
2369         <NA>                            <NA>
2370         <NA>                            <NA>
2371         <NA>                            <NA>
2372         <NA>                            <NA>
2373         <NA>                            <NA>
2374         <NA>                            <NA>
2375         <NA>                            <NA>
2376         <NA>                            <NA>
2377         <NA>                            <NA>
2378         <NA>                            <NA>
2379         <NA>                            <NA>
2380         <NA>              Grant,  Grant 1965
2381         <NA>              Grant,  Grant 1965
2382         <NA>              Grant,  Grant 1965
2383         <NA>              Grant,  Grant 1965
2384         <NA>              Grant,  Grant 1965
2385         <NA>              Grant,  Grant 1965
2386         <NA>              Grant,  Grant 1965
2387         <NA>              Grant,  Grant 1965
2388         <NA>              Grant,  Grant 1965
2389         <NA>              Grant,  Grant 1965
2390         <NA>              Grant,  Grant 1965
2391         <NA>              Grant,  Grant 1965
2392         <NA>              Grant,  Grant 1965
2393         <NA>              Grant,  Grant 1965
2394         <NA>              Grant,  Grant 1965
2395         <NA>              Grant,  Grant 1965
2396         <NA>              Grant,  Grant 1965
2397         <NA>              Grant,  Grant 1965
2398         <NA>              Grant,  Grant 1965
2399         <NA>              Grant,  Grant 1965
2400         <NA>              Grant,  Grant 1965
2401         <NA>              Grant,  Grant 1965
2402         <NA>              Grant,  Grant 1965
2403         <NA>              Grant,  Grant 1965
2404         <NA>              Grant,  Grant 1965
2405         <NA>              Grant,  Grant 1965
2406         <NA>              Grant,  Grant 1965
2407         <NA>              Grant,  Grant 1965
2408         <NA>              Grant,  Grant 1965
2409         <NA>              Grant,  Grant 1965
2410         <NA>              Grant,  Grant 1965
2411         <NA>              Grant,  Grant 1965
2412         <NA>              Grant,  Grant 1965
2413         <NA>              Grant,  Grant 1965
2414         <NA>              Grant,  Grant 1965
2415         <NA>              Grant,  Grant 1965
2416         <NA>              Grant,  Grant 1965
2417         <NA>              Grant,  Grant 1965
2418         <NA>              Grant,  Grant 1965
2419         <NA>              Grant,  Grant 1965
2420         <NA>              Grant,  Grant 1965
2421         <NA>              Grant,  Grant 1965
2422         <NA>              Grant,  Grant 1965
2423         <NA>              Grant,  Grant 1965
2424         <NA>              Grant,  Grant 1965
2425         <NA>              Grant,  Grant 1965
2426         <NA>              Grant,  Grant 1965
2427         <NA>              Grant,  Grant 1965
2428         <NA>              Grant,  Grant 1965
2429         <NA>              Grant,  Grant 1965
2430         <NA>              Grant,  Grant 1965
2431         <NA>              Grant,  Grant 1965
2432         <NA>                            <NA>
2433         <NA>                            <NA>
2434         <NA>                            <NA>
2435         <NA>                            <NA>
2436         <NA>                            <NA>
2437         <NA>                            <NA>
2438         <NA>                            <NA>
2439         <NA>                            <NA>
2440         <NA>                            <NA>
2441         <NA>                            <NA>
2442         <NA>                            <NA>
2443         <NA>                            <NA>
2444         <NA>                            <NA>
2445         <NA>                            <NA>
2446         <NA>                            <NA>
2447         <NA>                            <NA>
2448         <NA>                            <NA>
2449         <NA>                            <NA>
2450         <NA>                            <NA>
2451         <NA>                            <NA>
2452         <NA>                            <NA>
2453         <NA>                            <NA>
2454         <NA>                            <NA>
2455         <NA>                            <NA>
2456         <NA>                            <NA>
2457         <NA>                            <NA>
2458         <NA>                            <NA>
2459         <NA>                            <NA>
2460         <NA>                            <NA>
2461         <NA>                            <NA>
2462         <NA>                            <NA>
2463         <NA>                            <NA>
2464         <NA>                            <NA>
2465         <NA>                            <NA>
2466         <NA>                            <NA>
2467         <NA>                            <NA>
2468         <NA>                            <NA>
2469         <NA>                            <NA>
2470         <NA>                            <NA>
2471         <NA>                            <NA>
2472         <NA>              Grant,  Grant 1965
2473         <NA>              Grant,  Grant 1965
2474         <NA>              Grant,  Grant 1965
2475         <NA>              Grant,  Grant 1965
2476         <NA>              Grant,  Grant 1965
2477         <NA>              Grant,  Grant 1965
2478         <NA>              Grant,  Grant 1965
2479         <NA>              Grant,  Grant 1965
2480         <NA>              Grant,  Grant 1965
2481         <NA>              Grant,  Grant 1965
2482         <NA>              Grant,  Grant 1965
2483         <NA>              Grant,  Grant 1965
2484         <NA>              Grant,  Grant 1965
2485         <NA>              Grant,  Grant 1965
2486         <NA>              Grant,  Grant 1965
2487         <NA>              Grant,  Grant 1965
2488         <NA>              Grant,  Grant 1965
2489         <NA>                            <NA>
2490         <NA>                            <NA>
2491         <NA>                            <NA>
2492         <NA>                            <NA>
2493         <NA>                            <NA>
2494         <NA>                            <NA>
2495         <NA>                            <NA>
2496         <NA>                            <NA>
2497         <NA>                            <NA>
2498         <NA>                            <NA>
2499         <NA>                            <NA>
2500         <NA>                            <NA>
2501         <NA>                            <NA>
2502         <NA>                            <NA>
2503         <NA>                            <NA>
2504         <NA>                            <NA>
2505         <NA>                            <NA>
2506         <NA>              Grant,  Grant 1965
2507         <NA>              Grant,  Grant 1965
2508         <NA>              Grant,  Grant 1965
2509         <NA>              Grant,  Grant 1965
2510         <NA>              Grant,  Grant 1965
2511         <NA>              Grant,  Grant 1965
2512         <NA>              Grant,  Grant 1965
2513         <NA>              Grant,  Grant 1965
2514         <NA>              Grant,  Grant 1965
2515         <NA>              Grant,  Grant 1965
2516         <NA>              Grant,  Grant 1965
2517         <NA>              Grant,  Grant 1965
2518         <NA>              Grant,  Grant 1965
2519         <NA>              Grant,  Grant 1965
2520         <NA>              Grant,  Grant 1965
2521         <NA>              Grant,  Grant 1965
2522         <NA>              Grant,  Grant 1965
2523         <NA>              Grant,  Grant 1965
2524         <NA>              Grant,  Grant 1965
2525         <NA>              Grant,  Grant 1965
2526         <NA>              Grant,  Grant 1965
2527         <NA>              Grant,  Grant 1965
2528         <NA>              Grant,  Grant 1965
2529         <NA>              Grant,  Grant 1965
2530         <NA>              Grant,  Grant 1965
2531         <NA>              Grant,  Grant 1965
2532         <NA>              Grant,  Grant 1965
2533         <NA>              Grant,  Grant 1965
2534         <NA>              Grant,  Grant 1965
2535         <NA>              Grant,  Grant 1965
2536         <NA>              Grant,  Grant 1965
2537         <NA>              Grant,  Grant 1965
2538         <NA>              Grant,  Grant 1965
2539         <NA>              Grant,  Grant 1965
2540         <NA>              Grant,  Grant 1965
2541         <NA>              Grant,  Grant 1965
2542         <NA>              Grant,  Grant 1965
2543         <NA>              Grant,  Grant 1965
2544         <NA>              Grant,  Grant 1965
2545         <NA>              Grant,  Grant 1965
2546         <NA>              Grant,  Grant 1965
2547         <NA>              Grant,  Grant 1965
2548         <NA>              Grant,  Grant 1965
2549         <NA>              Grant,  Grant 1965
2550         <NA>              Grant,  Grant 1965
2551         <NA>              Grant,  Grant 1965
2552         <NA>              Grant,  Grant 1965
2553         <NA>              Grant,  Grant 1965
2554         <NA>              Grant,  Grant 1965
2555         <NA>              Grant,  Grant 1965
2556         <NA>              Grant,  Grant 1965
2557         <NA>              Grant,  Grant 1965
2558         <NA>              Grant,  Grant 1965
2559         <NA>              Grant,  Grant 1965
2560         <NA>              Grant,  Grant 1965
2561         <NA>              Grant,  Grant 1965
2562         <NA>              Grant,  Grant 1965
2563         <NA>              Grant,  Grant 1965
2564         <NA>              Grant,  Grant 1965
2565         <NA>              Grant,  Grant 1965
2566         <NA>              Grant,  Grant 1965
2567         <NA>                            <NA>
2568         <NA>                            <NA>
2569         <NA>                            <NA>
2570         <NA>                            <NA>
2571         <NA>                            <NA>
2572         <NA>                            <NA>
2573         <NA>                            <NA>
2574         <NA>              Grant,  Grant 1965
2575         <NA>              Grant,  Grant 1965
2576         <NA>              Grant,  Grant 1965
2577         <NA>              Grant,  Grant 1965
2578         <NA>              Grant,  Grant 1965
2579         <NA>              Grant,  Grant 1965
2580         <NA>              Grant,  Grant 1965
2581         <NA>              Grant,  Grant 1965
2582         <NA>              Grant,  Grant 1965
2583         <NA>              Grant,  Grant 1965
2584         <NA>              Grant,  Grant 1965
2585         <NA>              Grant,  Grant 1965
2586         <NA>              Grant,  Grant 1965
2587         <NA>              Grant,  Grant 1965
2588         <NA>              Grant,  Grant 1965
2589         <NA>              Grant,  Grant 1965
2590         <NA>              Grant,  Grant 1965
2591         <NA>              Grant,  Grant 1965
2592         <NA>                            <NA>
2593         <NA>                            <NA>
2594         <NA>                            <NA>
2595         <NA>                            <NA>
2596         <NA>                            <NA>
2597         <NA>                            <NA>
2598         <NA>                            <NA>
2599         <NA>                            <NA>
2600         <NA>                            <NA>
2601         <NA>                            <NA>
2602         <NA>                            <NA>
2603         <NA>                            <NA>
2604         <NA>                            <NA>
2605         <NA>                            <NA>
2606         <NA>                            <NA>
2607         <NA>                            <NA>
2608         <NA>              Grant,  Grant 1965
2609         <NA>              Grant,  Grant 1965
2610         <NA>              Grant,  Grant 1965
2611         <NA>                            <NA>
2612         <NA>                            <NA>
2613         <NA>                            <NA>
2614         <NA>                            <NA>
2615         <NA>                            <NA>
2616         <NA>                            <NA>
2617         <NA>                            <NA>
2618         <NA>                            <NA>
2619         <NA>                            <NA>
2620         <NA>              Grant,  Grant 1965
2621         <NA>              Grant,  Grant 1965
2622         <NA>              Grant,  Grant 1965
2623         <NA>              Grant,  Grant 1965
2624         <NA>              Grant,  Grant 1965
2625         <NA>              Grant,  Grant 1965
2626         <NA>              Grant,  Grant 1965
2627         <NA>              Grant,  Grant 1965
2628         <NA>              Grant,  Grant 1965
2629         <NA>              Grant,  Grant 1965
2630         <NA>              Grant,  Grant 1965
2631         <NA>              Grant,  Grant 1965
2632         <NA>              Grant,  Grant 1965
2633         <NA>              Grant,  Grant 1965
2634         <NA>              Grant,  Grant 1965
2635         <NA>              Grant,  Grant 1965
2636         <NA>              Grant,  Grant 1965
2637         <NA>              Grant,  Grant 1965
2638         <NA>              Grant,  Grant 1965
2639         <NA>              Grant,  Grant 1965
2640         <NA>              Grant,  Grant 1965
2641         <NA>              Grant,  Grant 1965
2642         <NA>              Grant,  Grant 1965
2643         <NA>              Grant,  Grant 1965
2644         <NA>              Grant,  Grant 1965
2645         <NA>              Grant,  Grant 1965
2646         <NA>              Grant,  Grant 1965
2647         <NA>              Grant,  Grant 1965
2648         <NA>              Grant,  Grant 1965
2649         <NA>              Grant,  Grant 1965
2650         <NA>              Grant,  Grant 1965
2651         <NA>              Grant,  Grant 1965
2652         <NA>              Grant,  Grant 1965
2653         <NA>              Grant,  Grant 1965
2654         <NA>              Grant,  Grant 1965
2655         <NA>              Grant,  Grant 1965
2656         <NA>              Grant,  Grant 1965
2657         <NA>              Grant,  Grant 1965
2658         <NA>              Grant,  Grant 1965
2659         <NA>              Grant,  Grant 1965
2660         <NA>              Grant,  Grant 1965
2661         <NA>              Grant,  Grant 1965
2662         <NA>              Grant,  Grant 1965
2663         <NA>              Grant,  Grant 1965
2664         <NA>              Grant,  Grant 1965
2665         <NA>              Grant,  Grant 1965
2666         <NA>              Grant,  Grant 1965
2667         <NA>              Grant,  Grant 1965
2668         <NA>              Grant,  Grant 1965
2669         <NA>              Grant,  Grant 1965
2670         <NA>              Grant,  Grant 1965
2671         <NA>              Grant,  Grant 1965
2672         <NA>              Grant,  Grant 1965
2673         <NA>              Grant,  Grant 1965
2674         <NA>              Grant,  Grant 1965
2675         <NA>              Grant,  Grant 1965
2676         <NA>              Grant,  Grant 1965
2677         <NA>              Grant,  Grant 1965
2678         <NA>              Grant,  Grant 1965
2679         <NA>              Grant,  Grant 1965
2680         <NA>              Grant,  Grant 1965
2681         <NA>              Grant,  Grant 1965
2682         <NA>              Grant,  Grant 1965
2683         <NA>              Grant,  Grant 1965
2684         <NA>              Grant,  Grant 1965
2685         <NA>              Grant,  Grant 1965
2686         <NA>                            <NA>
2687         <NA>                            <NA>
2688         <NA>                            <NA>
2689         <NA>                            <NA>
```
