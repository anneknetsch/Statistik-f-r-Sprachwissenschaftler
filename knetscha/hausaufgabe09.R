# Hausaufgabe 09
# Anne Kathrin Knetsch <knetscha@students.uni-marburg.de>
# 2014-05-13
# Diese Datei darf nur Prüfungszwecken dienen.


# Die nächsten Punkte sollten langsam automatisch sein...
# 1. Kopieren Sie diese Datei in Ihren Ordner (das können Sie innerhalb RStudio machen 
#    oder mit Explorer/Finder/usw.) und öffnen Sie die Kopie. Ab diesem Punkt arbeiten 
#    Sie mit der Kopie. Die Kopie bitte `hausaufgabe09.R` nennen und nicht `Kopie...`
# 2. Sie sehen jetzt im Git-Tab, dass der neue Ordner als unbekannt (mit gelbem Fragezeichen)
#    da steht. Geben Sie Git Bescheid, dass Sie die Änderungen im Ordner verfolgen möchten 
#    (auf Stage klicken). Die neue Datei steht automatisch da.
# 3. Machen Sie ein Commit mit den bisherigen Änderungen (schreiben Sie eine sinnvolle 
#    Message dazu -- sinnvoll bedeutet nicht unbedingt lang) und danach einen Push.
# 4. Ersetzen Sie meinen Namen oben mit Ihrem. Klicken auf Stage, um die Änderung zu merken.
# 5. Ändern Sie das Datum auf heute. (Seien Sie ehrlich! Ich kann das sowieso am Commit sehen.)
# 6. Sie sehen jetzt, dass es zwei Symbole in der Status-Spalte gibt, eins für den Zustand 
#    im *Staging Area* (auch als *Index* bekannt), eins für den Zustand im Vergleich zum 
#    Staging Area. Sie haben die Datei modifiziert, eine Änderung in das Staging Area aufgenommen,
#    und danach weitere Änderungen gemacht. Nur Änderungen im Staging Area werden in den Commit aufgenommen.
# 7. Stellen Sie die letzten Änderungen auch ins Staging Area und machen Sie einen Commit 
#    (immer mit sinnvoller Message!).
# 8. Vergessen Sie nicht am Ende, die Lizenz ggf. zu ändern!

# Um einiges leichter zu machen, sollten Sie auch die
# Datei punnkt_rt.tab aus dem Data-Ordner kopieren, stagen und commiten. Sie
# müssen ggf. Ihr Arbeitsverzeichnis setzen, wenn R die .tab-Datei nicht finden
# kann: 
# Session > Set Working Directory > Source File Location

# (Im folgenden müssen Sie die Code-Zeilen wieder aktiv setzen -- ich habe sie
# vorläufig auskommentiert, damit der Output beim ersten Beispiel sehr
# überschaubar war.)

# Weil wir uns immer die Daten auch grafisch anschauen, laden wir jetzt schon ggplot
library(ggplot2)
# car laden wir auch für später
library(car)
# car steht übrigens für "Companion to Appled Regression"

# und danach die Daten:
rt <- read.table("punkt_rt.tab",header=TRUE) 
# Die Daten sind Reaktionszeiten von zwei Versuchspersonen auf einen weißen
# Punkt auf einem schwarzen Bildschirm. Die Verzögerung (delay) zwischen Trials
# (Läufen) war zufällig und mitaugenommen. 

# (Wie würden Sie abschneiden? Wenn Sie wollen, können Sie das Experiment (im
# Data-Ordner) mit Hilfe der open source Software OpenSesame
# (http://osdoc.cogsci.nl/) auch zu Hause ausführen!)

# Wir schauen uns erst mal eine Zusammenfassung der Daten an:
print(summary(rt))

# Wir sehen sofort, dass R die Variabel "subj" als numerische Variable
# behandelt hat, obwohl sie eigentlich kategorisch ist. Das müssen wir ändern:
rt$subj <- as.factor(rt$subj)
# 
rt.plot <- qplot(x=RT,color=subj,fill=subj,data=rt, geom="density",alpha=I(0.3))
print(rt.plot)

# Haben die Daten der beiden Gruppen -- die wiederholten Messwerte der einzelnen
# Probanden bilden ja Gruppen -- homogene Varianz? Bevor Sie irgendwelche Tests 
# ausführen, schauen Sie sich nochmal die Grafik an. Haben beide Verteilungen in
# etwa die gleiche Streuung, auch wenn sich die Modalwerte (grafisch: höchste 
# Gipfel) unterscheiden? Sind die Gruppen beide normal verteilt? Hier müssen Sie
# keine Antwort geben, aber Sie sollten schon auf ein Stück Schmierpapier schreiben,
# was Sie da denken. Sie können dann nämlich sich selber nicht täuschen, dass
# Sie von vorneherein etwas behaupten haben.

# Berechnen Sie jetzt den F-Test:
print(var.test(rt$RT[1:10],rt$RT[11:20]))
# ODER
print(var.test(rt$RT~rt$subj))

# Sind die Varianzen homogen? Vergessen Sie nicht, dass die Nullhypothese beim
# F-Test "Varianzen Ungleich" ist.

# Da Nullhypothese abgelehnt werden muss sind Varianzen homogen.

# Berechenen Sie den Levene Test:#
print(leveneTest(rt$RT~rt$subj))

# Sind die Varianzen homogen? Vergessen Sie nicht, dass die Nullhypothese beim
# Levene Test "Varianzen Gleich" ist.

# Nullhypothese wird beibehalten; Varianzen sind homogen.

# Für heterogene Varianzen haben wir eine Variante des  t-Tests gesehen, die
# eine Korrektur der Freiheitsgerade macht. Bei homogener Varianz sollten beide
# Variante ähnliche bzw. (fast) gleiche Ergebnisse liefern. Ist das hier der
# Fall?

two.sample <- t.test(rt$RT[1:10],rt$RT[11:20],var.equal=TRUE)

welch <- t.test(rt$RT[1:10],rt$RT[11:20])

print(two.sample)
print(welch)

# Sieht ziemlich ähnlich aus!

# Das Ergebnis der verschiedenen Test-Funktionen in R ist übrigens eine Liste.
# Wir können das ausnutzen, um zu schauen, ob es einen Unterschied zwischen den
# beiden Testverfahren gab. Wenn die Varianz homogen war, sollten wir keinen
# Unterschied sehen:
t.diff <- welch$statistic - two.sample$statistic
print(paste("Die Differenz zwischen den beiden t-Werten ist",t.diff,"."))

# Differenz zwischen beiden t-Werten ist 0, also ist die Varianz homogen.

# Sind die Daten normal verteilt? Wir berechnen Sie den Shapiro Test für erste Versuchsperson:
shapiro <- shapiro.test(rt[rt$subj==1,"RT"])
# 
print(shapiro)

# Wir können auch "Entscheidungen" im Code treffen. Die Syntax dafür ist wie
# folgt -- die runden und geschweiften Klammern sind alle sehr wichtig!
if (shapiro$p.value > 0.05){print("Shapiro's test insignifikant, die Daten sind normal verteilt.")}else{print("Shapiro's test signifikant, die Daten sind nicht normal verteilt.")}


# Berechnen Sie Shapiro's Test für die andere Versuchsperson und drücken Sie mit
# einem if-Block aus, ob die Daten normal verteilt sind.

shapiro2 <- shapiro.test(rt[rt$subj==2,"RT"])
print(shapiro2)
#ODER
shapiro2 <- shapiro.test(rt$RT[11:20])
print(shapiro2)

#if-Block
if (shapiro2$p.value > 0.05){print("Shapiro's test insignifikant, die Daten sind normal verteilt.")}else{print("Shapiro's test signifikant, die Daten sind nicht normal verteilt.")}


# Wir haben auch Transformationen bei schiefen Datenverteilungen angesprochen.
# Die logaritmische Verteilung ist ziemlich beliebt bei Reaktionszeitsdaten.

rt$logRT <- log(rt$RT)
print(summary(rt$logRT))
print(rt$logRT)
logrt.plot <- qplot(x=logRT,color=subj,fill=subj,data=rt, geom="density",alpha=I(0.3))
print(logrt.plot)

# Sieht die Verteilung besser aus? Sind die Varianzen "homogener" geworden? 
# Berechnen Sie den F-Test und den Levene-Test für die logaritmisch skalierten 
# Daten. Nach jedem Test sollten Sie auch programmatisch (=durch if-Blöcke)
# ausdrücken, ob die Varianzen homogen sind.

# F-Test
print(var.test(rt$logRT[1:10],rt$logRT[11:20]))

# Levene Test
leveneTest(rt$logRT~rt$subj)

# Sind die Daten "normaler" gewordern? Berechnen Sie den Shapiro-Test für beide 
# Gruppen. Nach jeder Gruppe sollten Sie auch programmatisch (=durch if-Blöcke)
# ausdrücken, ob die Daten normal verteilt sind. 
# (Für die fortgeschrittenen: hier könnte man auch eine for-Schleife nutzen...)

# Shapiro Test Subject 1
shapirolog1 <- shapiro.test(rt$logRT[1:10])
print(shapirolog1)

if (shapirolog1$p.value > 0.05){print("Shapiro's test insignifikant, die Daten sind normal verteilt.")}else{print("Shapiro's test signifikant, die Daten sind nicht normal verteilt.")}

#shapiro Test Subject 2
shapirolog2 <- shapiro.test(rt$logRT[11:20])
print(shapirolog2)

if (shapirolog2$p.value > 0.05){print("Shapiro's test insignifikant, die Daten sind normal verteilt.")}else{print("Shapiro's test signifikant, die Daten sind nicht normal verteilt.")}

# Die Daten sind eher nicht normaler geworden. Voher wurde nur subject 1 als normal verteilt beschrieben,
# jetzt werden beide Versuchspersonen als nicht normal verteilt beschrieben.Der p-Wert der 
# zweiten Versuchsperson hat sich kaum verändert.

# Hat die logarithmische Transformation insgesamt geholfen? Berechnen Sie zum
# Schluss den (Welch) t-Test für die logarithmischen Daten. Bekommen Sie das
# gleiche Ergebnisse wie bei den Ausgangsdaten?

two.sample.log <- t.test(rt$logRT[1:10],rt$logRT[11:20],var.equal=TRUE)

welchlog <- t.test(rt$logRT[1:10],rt$logRT[11:20])

print(two.sample.log)
print(welchlog)

t.diff <- welchlog$statistic - two.sample.log$statistic
print(paste("Die Differenz zwischen den beiden t-Werten ist",t.diff,"."))

# Der P-Wert ist etwas größer, das heißt Ergebnis ist etwas weniger signifikant. Man kommt 
# jedoch wiederum (durch Vergleich two sample t test/ welch two sample t test) 
# auf das Ergebnis, dass die Varianzen homogen sind (Differenz=0).


