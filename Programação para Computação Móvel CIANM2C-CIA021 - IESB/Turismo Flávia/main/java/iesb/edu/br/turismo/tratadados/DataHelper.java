package iesb.edu.br.turismo.tratadados;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

public class DataHelper extends SQLiteOpenHelper {

    private static final String DATABASE_NAME = "monumento.db";
    private static final String TABLE_NAME = "brasilia";
    private static final int VERSAO = 1;
    private SQLiteDatabase db;

    public DataHelper(@Nullable Context context) {
        super(context, DATABASE_NAME, null, VERSAO);
        this.db = this.getWritableDatabase();
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        db.execSQL("CREATE TABLE " + TABLE_NAME +
                " (id INTEGER PRIMARY KEY AUTOINCREMENT," +
                " nomeImg TEXT, descricao TEXT, historia TEXT, inauguracao TEXT)");
        db.execSQL("INSERT INTO " + TABLE_NAME +
                " (nomeImg, descricao, historia, inauguracao) " +
                "VALUES ('congresso', 'Congresso Nacional', " +
                "'O Congresso Nacional é o titular do Poder Legislativo Federal, e o exerce por meio da Câmara dos Deputados e do Senado Federal, cabendo-lhe legislar sobre as matérias de competência da União bem como fiscalizar as entidades da administração direta e indireta, com o auxílio do Tribunal de Contas da União.', " +
                "'21 de abril de 1960')");
        db.execSQL("INSERT INTO " + TABLE_NAME +
                " (nomeImg, descricao, historia, inauguracao) " +
                "VALUES ('itamaraty', 'Palácio do Itamaraty', " +
                "'O Palácio do Itamaraty é a sede do Ministério das Relações Exteriores e foi concebido para apresentar o Brasil aos visitantes estrangeiros. Por isso, foi construído apenas com materiais nacionais e seus salões abrigam obras apenas de artistas nascidos ou naturalizados brasileiros. O projeto é de Oscar Niemeyer e o paisagismo é de autoria de Roberto Burle Marx.'," +
                "'20 de abril de 1970')");
        db.execSQL("INSERT INTO " + TABLE_NAME +
                " (nomeImg, descricao, historia, inauguracao) " +
                "VALUES ('museu', 'Museu Nacional', " +
                "'O Museu Nacional é integrante do Conjunto Cultural da República. É um espaço que insere Brasília no circuito internacional das artes e mostra o que há de melhor na arte brasileira. O espaço é utilizado para exposições itinerantes de artistas renomados e temas importantes para a sociedade, palestras, mostra de filmes, seminários e eventos importantes.', " +
                "'15 de dezembro de 2006')");
        db.execSQL("INSERT INTO " + TABLE_NAME +
                " (nomeImg, descricao, historia, inauguracao) " +
                "VALUES ('ponte', 'Ponte Juscelino Kubitschek', " +
                "'Mais conhecida como Ponte JK ou Terceira Ponte, liga o Lago Sul, Paranoá e São Sebastião à parte central de Brasília, através do Eixo Monumental, atravessando o Lago Paranoá. Inaugurada em 15 de dezembro de 2002, a estrutura da ponte tem um comprimento de travessia total de 1,2 quilômetros, tendo largura de 24 metros com duas pistas, cada uma com três faixas de rolamento, duas passarelas nas laterais para uso de ciclistas e pedestres com 1,5 metros de largura e comprimento total dos vãos de 720 metros.', " +
                "'15 de dezembro de 2002')");
        db.execSQL("INSERT INTO " + TABLE_NAME +
                " (nomeImg, descricao, historia, inauguracao) " +
                "VALUES ('memorial', 'Memorial JK', " +
                "'O Memorial JK é um museu, mausoléu e centro cultural brasileiro construído para homenagear o 21º presidente do Brasil, Juscelino Kubistchek de Oliveira.', " +
                "'12 de setembro de 1981')");
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {

    }

    // Padrão DAO - Data Access Object

    public List<MonumentoDTO> getAllMonumentos() {
        List<MonumentoDTO> lst = new ArrayList<>();
        Cursor res = db.rawQuery(
            "SELECT id, nomeImg, descricao, historia, inauguracao FROM " +
            TABLE_NAME, null);
        if (res.moveToFirst()) {
            while (!res.isAfterLast()) {
                lst.add(new MonumentoDTO(
                        res.getInt(0),
                        res.getString(1),
                        res.getString(2),
                        res.getString(3),
                        res.getString(4))
                );
                res.moveToNext();
            }
        }
        res.close();
        return lst;
    }
}
