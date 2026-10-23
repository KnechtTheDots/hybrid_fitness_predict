rule render:
    input:
        "notebooks/notebook1.qmd"
    output:
        "notebooks/notebook1.html"
    log:
        "results/notebook1.log"
    conda:
        "envs/environment.yaml"
    shell:
        """
        (quarto render {input} --to html) 2> {log}
        """
