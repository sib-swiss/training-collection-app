#!/usr/bin/env python3

import re

def parse_readme(readme:str, outfile:str):
    outf = open(outfile, "w")

    with open(readme, "r") as fil:
        for line in fil:
            gh_link_match = re.search("\(https\:\/\/github.com\/([a-zA-Z0-9-_]+\/[a-zA-Z0-9-/_]+)\)", line)
            if(gh_link_match):
                gh_slug = gh_link_match.group(1)
                outf.write(re.sub("/$", "", gh_slug) + "\n")
    outf.close()
     
def main():
    parse_readme("README.md", "gh_slugs.txt")

if __name__ == "__main__":
    main()