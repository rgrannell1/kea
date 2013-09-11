
require (arrow)
require (needy)

opts <- list(
	target_dir = 
		'/home/rgrannell1/Desktop/packages'
)

#------------------ CRAN data.

cran_mirror <- ( function () {
	# return a function that returns a random cran mirror.

	cran_mirrors <- getCRANmirrors()[,'URL']
	function () {
		sample(cran_mirrors, size = 1)
	}
} )()

cran_raw_data <- utils::available.packages()
cran_data <- xZipWith(
	xAsVariadic(row := {

		list(
			package = row[[1]], depends = row[[2]],
			imports = row[[3]], license = row[[4]],
			suggests = row[[5]], enhances = row[[6]],
			linking_to = row[[7]] )

	}),
	cran_raw_data[,'Package'],
	cran_raw_data[,'Depends'],
	cran_raw_data[,'Imports'],
	cran_raw_data[,'License'],
	cran_raw_data[,'Suggests'],
	cran_raw_data[,'Enhances'],
	cran_raw_data[,'LinkingTo']
)

#------------------ Accessory functions used below.

select_field <- field := {
	# get a field in a row of cran data.

	x_(cran_data)$
	xMap(row := row[[field]] )$
	xReject(xIsNa)$
	x()
}

grab_package <- package := {
	# download each package as a tarball.

	try(download.packages(
		package, repos = cran_mirror(),
		destdir = opts$target_dir), silent = True)
}

is_targz <- path := {
	grepl('[.]tar[.]gz$', path)
}

untarball <- path := {
	wd <- getwd()
	setwd(opts$target_dir)
		untar(path)
	setwd(wd)
}

#------------------ Act I: Grab All R code on CRAN.

#--- grab each package

x_(select_field('package'))$xDo(grab_package)

#--- grab the .R files in each package

files <- list.files(
	opts$target_dir,
	full.names = True)

tarballs <- xSelect(is_targz, files)

x_( tarballs )$xDo(untarball)

files <- list.files(
	opts$target_dir,
	full.names = True)

folders <- xReject(is_targz, files)

#--- recurse into each folder, grabbing R files.

package_source_paths <- x_(folders)$xMap(path := {

	list.files(
		path,
		pattern = '[.]R$|[.]r$', 
		full.names = True,
		recursive = True)

})$x()

# grab each 
read_lines <- path := {
	paste0(readLines(path, warn = 'false'), collapse = '\n')
}

r_source <- x_(package_source_paths)$xMap(paths := {
	x_(paths)$xMap(read_lines)$x()
})$x()
names(r_source) <-
	x_(folders)$xMap(path := {
		xLast( xSplitStr("/", path) )
	} )$x()

#------------------ Act II: Count the occurrences of nasty words.
is_rude <- ( function () {
	swears <- x_(c(
		"2g1c", "2 girls 1 cup", "acrotomophilia", "anal", "anilingus", "anus", 
		"arsehole", "ass", "asshole", "assmunch", "auto erotic", "autoerotic", "babeland", 
		"baby batter", "ball gag", "ball gravy", "ball kicking", "ball licking", "ball sack", 
		"ball sucking", "bangbros", "bareback", "barely legal", "barenaked", "bastardo", 
		"bastinado", "bbw", "bdsm", "beaver cleaver", "beaver lips", "bestiality", 
		"bi curious", "big black", "big breasts", "big knockers", "big tits", "bimbos", 
		"birdlock", "bitch", "black cock", "blonde action", "blonde on blonde action", 
		"blow j", "blow your l", "blue waffle", "blumpkin", "bollocks", "bondage", 
		"boner", "boob", "boobs", "booty call", "brown showers", "brunette action", 
		"bukkake", "bulldyke", "bullet vibe", "bung hole", "bunghole", "busty", "butt", 
		"buttcheeks", "butthole", "camel toe", "camgirl", "camslut", "camwhore", 
		"carpet muncher", "carpetmuncher", "chocolate rosebuds", "circlejerk", 
		"cleveland steamer", "clit", "clitoris", "clover clamps", "clusterfuck", "cock", 
		"cocks", "coprolagnia", "coprophilia", "cornhole", "cum", "cumming", 
		"cunnilingus", "cunt", "darkie", "date rape", "daterape", "deep throat", 
		"deepthroat", "dick", "dildo", "dirty pillows", "dirty sanchez", "dog style", 
		"doggie style", "doggiestyle", "doggy style", "doggystyle", "dolcett", "domination", 
		"dominatrix", "dommes", "donkey punch", "double dong", "double penetration", 
		"dp action", "eat my ass", "ecchi", "ejaculation", "erotic", "erotism", "escort", 
		"ethical slut", "eunuch", "faggot", "fecal", "felch", "fellatio", "feltch", 
		"female squirting", "femdom", "figging", "fingering", "fisting", "foot fetish", "footjob", 
		"frotting", "fuck", "fuck buttons", "fudge packer", "fudgepacker", "futanari", "g-spot", 
		"gang bang", "gay sex", "genitals", "giant cock", "girl on", "girl on top", "girls gone wild", 
		"goatcx", "goatse", "gokkun", "golden shower", "goo girl", "goodpoop", 
		"goregasm", "grope", "group sex", "guro", "hand job", "handjob", "hard core", 
		"hardcore", "hentai", "homoerotic", "honkey", "hooker", "hot chick", "how to kill", 
		"how to murder", "huge fat", "humping", "incest", "intercourse", "jack off", 
		"jail bait", "jailbait", "jerk off", "jigaboo", "jiggaboo", "jiggerboo", "jizz", "juggs", 
		"kike", "kinbaku", "kinkster", "kinky", "knobbing", "leather restraint", 
		"leather straight jacket", "lemon party", "lolita", "lovemaking", "make me come", 
		"male squirting", "masturbate", "menage a trois", "milf", "missionary position", 
		"motherfucker", "mound of venus", "mr hands", "muff diver", "muffdiving", "nambla", 
		"nawashi", "negro", "neonazi", "nig nog", "nigga", "nigger", "nimphomania", 
		"nipple", "nipples", "nsfw images", "nude", "nudity", "nympho", "nymphomania", 
		"octopussy", "omorashi", "one cup two girls", "one guy one jar", "orgasm", "orgy", 
		"paedophile", "panties", "panty", "pedobear", "pedophile", "pegging", "penis", 
		"phone sex", "piece of shit", "piss pig", "pissing", "pisspig", "playboy", 
		"pleasure chest", "pole smoker", "ponyplay", "poof", "poop chute", "poopchute", 
		"porn", "porno", "pornography", "prince albert piercing", "pthc", "pubes", 
		"pussy", "queaf", "raghead", "raging boner", "rape", "raping", "rapist", 
		"rectum", "reverse cowgirl", "rimjob", "rimming", "rosy palm", 
		"rosy palm and her 5 sisters", "rusty trombone", "s&m", "sadism", "scat", 
		"schlong", "scissoring", "semen", "sex", "sexo", "sexy", "shaved beaver", 
		"shaved pussy", "shemale", "shibari", "shit", "shota", "shrimping", 
		"slanteye", "slut", "smut", "snatch", "snowballing", "sodomize", "sodomy", 
		"spic", "spooge", "spread legs", "strap on", "strapon", "strappado", 
		"strip club", "style doggy", "suck", "sucks", "suicide girls", "sultry women", 
		"swastika", "swinger", "tainted love", "taste my", "tea bagging", "threesome", 
		"throating", "tied up", "tight white", "tit", "tits", "titties", "titty", "tongue in a",
		 "topless", "tosser", "towelhead", "tranny", "tribadism", "tub girl", "tubgirl", 
		 "tushy", "twat", "twink", "twinkie", "two girls one cup", "undressing", "upskirt", 
		 "urethra play", "urophilia", "vagina", "venus mound", "vibrator", "violet blue", 
		 "violet wand", "vorarephilia", "voyeur", "vulva", "wank", "wet dream", "wetback", 
		 "white power", "women rapping", "wrapping men", "wrinkled starfish", "xx", "xxx", 
		 "yaoi", "yellow showers", "yiffy", "zoophilia"))$x()

	function (line) {
		is.element(xWords(line), swears)
	}
} )()

#------ extract every swear word in a package & 
#------ make a freq-table by package.

swear_by_package <- 
	x_(r_source)$
	xMap(package := {

		x_(package)$
		xFlatten(1)$
		xLines()$
		xSelect(is_rude)$x()

	})$x()
