package iesb.edu.br.turismo;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.GridView;
import android.widget.TextView;
import android.widget.Toast;

import java.util.List;

import iesb.edu.br.turismo.tratadados.DataHelper;
import iesb.edu.br.turismo.tratadados.MonumentoDTO;

public class MainActivity extends AppCompatActivity {

    private GridView grid;
    private List<MonumentoDTO> lista;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        grid = findViewById(R.id.gridView1);
        DataHelper dh = new DataHelper(this);
        lista = dh.getAllMonumentos();
        CustomAdapter customAdapter = new CustomAdapter(this, R.layout.activity_detalhe, lista);
        grid.setAdapter(customAdapter);

        // Ao clicar em um Monumento
        grid.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                aoClicarMonumento(position);
            }
        });
    }

    private void aoClicarMonumento(int posicao) {
        // Aqui deve aparecer a Data de Construção em um Toast
        Toast.makeText(this, lista.get(posicao).getInauguracao(), Toast.LENGTH_SHORT).show();
    }
}