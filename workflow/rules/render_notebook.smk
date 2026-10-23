rule render:
    input:
        "notebooks/notebook1.qmd"
    output:
        "notebooks/notebook1.pdf"
    log:
        "results/notebook1.log"
    conda:
        "envs/environment.yaml"
    shell:
        """
        (quarto render {input} --to pdf) 2> {log}
        """
