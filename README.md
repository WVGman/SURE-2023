Welcome! This repo is archived, however I am continuing this research in my [ACA-Research repo]([url](https://github.com/gpk00003/ACA-Research)) during the Fall of 2023 and possibly beyond.

I'm Grady King, and this is my repository for all the work I did during the WVU Summer Undergraduate Research Experience during the summer of 2023. I worked under Dr. Srinivas Palanki, investigating the impact of the Affordable Care Act, and specifically the Medicaid expansions, on preventable heart disease mortality. As a disclaimer, I am a beginner at R, only using it since January, so my code might be suboptimal, and I am also not a statistician or econometrician, so my methodology might be shaky. I think I got better as I went through the project though.

Check out my final poster in `poster/` if you're interested in this research! 

The main two causal inference estimators I used were a staggered two-stage difference-in-difference (did2s) for nationwide analysis and synthetic difference-in-difference (synthdid) for the adjacent state analyses due to its robustness in smaller settings.

## Code Organization
This project is not fantastically organized, but it is separated okay-ish. All of my R source files are in the main folder, along with my RStudio project file and associated files with that. The `figures/` folder has all of the plots and maps I generated during the project, and the poster/ folder has all the files I used to make my poster for the summer undergrad symposium. `rawData/` has some raw files from certain sources, which should all have a link.txt file describing where I got the data. `formattedData/` is kind of a weird folder, because it has some imported data from my previous project during the spring, as well as some cleaned data that I created during the project. Most of my R code uses data from this folder, or formats data to be put into this folder. If you have any questions on where I obtained some of this data, feel free to email me!

`notes/` has personal notes that I wrote for each week, some of which are probably pretty whiny, I wouldn't recommend reading them. `renv/` is a folder that contains renv information, which essentially keeps track of all the libraries I used during the project so that other people can easily download them. `adjacentCountiesData/`, `baconDecomp/`, and `socialFactorData/` are all small subsections of the project or minor offshoots, adjacent counties results are in figures/comparing adjacent states, and social factor results are in figures/social factor plots. baconDecomp just has some results of a Bacon decomposition, which was just one of my side projects to understand two-way fixed effects (TWFE) more.

# Contact
Email: gpk00003@mix.wvu.edu

