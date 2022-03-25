package iesb.edu.br.turismo.tratadados;

// Padrão DTO - Data Transfer Object
public class MonumentoDTO {

    private int id;
    private String nomeImg;
    private String descricao;
    private String historia;
    private String inauguracao;

    public MonumentoDTO() {
    }

    public MonumentoDTO(int id, String nomeImg, String descricao, String historia, String inauguracao) {
        this.setId(id);
        this.setNomeImg(nomeImg);
        this.setDescricao(descricao);
        this.setHistoria(historia);
        this.setInauguracao(inauguracao);
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getNomeImg() {
        return nomeImg;
    }

    public void setNomeImg(String nomeImg) {
        this.nomeImg = nomeImg;
    }

    public String getDescricao() {
        return descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public String getHistoria() {
        return historia;
    }

    public void setHistoria(String historia) {
        this.historia = historia;
    }

    public String getInauguracao() {
        return inauguracao;
    }

    public void setInauguracao(String inauguracao) {
        this.inauguracao = inauguracao;
    }
}
